package freechips.rocketchip.devices.tilelink

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.TruthTable
import chisel3.experimental.{IntParam, BaseModule, ChiselEnum}
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.subsystem.BaseSubsystem
import freechips.rocketchip.config.{Parameters, Field, Config}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.{HasRegMap, RegField}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.UIntIsOneOf

case class MontgomeryParams(
  baseAddress: BigInt,
  useAXI4: Boolean = false,
  width: Int = 32,
  addPipe: Int = 10)

case object MontgomeryKey extends Field[Option[MontgomeryParams]](None)

trait HasMontgomeryIO extends BaseModule {
  val pWidth: Int

  val p = IO(Input(UInt(pWidth.W)))
  val pPrime = IO(Input(Bool()))
  val a = IO(Input(UInt(pWidth.W)))
  val b = IO(Input(UInt(pWidth.W)))
  val input_width = IO(Input(UInt(pWidth.W)))
  val valid = IO(Input(Bool())) // input valid
  val out = IO(Output(UInt(pWidth.W)))
  val out_valid = IO(Output(Bool())) // output valid
}

class MontgomeryMMIOChiselModule(val pWidth: Int, addPipe: Int) extends Module
  with HasMontgomeryIO
{
  val b_add_p = Reg(UInt((pWidth + 1).W))
  val u = Reg(Bool())
  val i = Reg(UInt((pWidth).W))
  val nextT = Reg(UInt((pWidth + 2).W))

  // multicycle prefixadder
  val adder = Module(new DummyAdd(pWidth + 2, addPipe))
  val add_stable = RegInit(0.U((pWidth + 2).W))
  // Control Path
  object StateType extends ChiselEnum {
    val s0 = Value("b0000001".U) // nextT = 0, u = a(0)b(0)pPrime
    // loop
    val s1 = Value("b0000010".U) // nextT + b
    val s2 = Value("b0000100".U) // nextT + p
    val s3 = Value("b0001000".U) // nextT + b_add_p
    // loop done
    val s4 = Value("b0010000".U) // i << 1, u = (nextT(0) + a(i)b(0))pPrime, nextT / 2
    val s5 = Value("b0100000".U) // if-then
    val s6 = Value("b1000000".U) // done
    val s7 = Value("b10000000".U) // nextT + 0
  }

  val state = RegInit(StateType.s0)
  val isAdd = (state.asUInt & "b10101111".U).orR
  adder.valid := isAdd
  val addDoneNext = RegInit(false.B)
  addDoneNext := addDone
  lazy val addDone = if (addPipe != 0) Counter(isAdd && (~addDoneNext), addPipe + 1)._2 else true.B
  val addSign = ((add_stable >> 1) < p.asUInt)
  val a_i = Reg(Bool())
  val iBreak = (i.asUInt >= input_width.asUInt)
  state := chisel3.util.experimental.decode
    .decoder(
      state.asUInt() ## addDoneNext ## valid ## i.head(1) ## iBreak ## addSign ## u ## a_i, {
        val Y = "1"
        val N = "0"
        val DC = "?"
        def to(
          stateI:  String,
          addDone: String = DC,
          valid:   String = DC,
          iHead:   String = DC,
          iBreak:  String = DC,
          addSign: String = DC,
          u:       String = DC,
          a_i:     String = DC
        )(stateO:  String
        ) = s"$stateI$addDone$valid$iHead$iBreak$addSign$u$a_i->$stateO"
        val s0 = "00000001"
        val s1 = "00000010"
        val s2 = "00000100"
        val s3 = "00001000"
        val s4 = "00010000"
        val s5 = "00100000"
        val s6 = "01000000"
        val s7 = "10000000"
        TruthTable.fromString(
          Seq(
            to(s0, valid = N)(s0),
            to(s0, valid = Y, addDone = N)(s0),
            to(s0, valid = Y, addDone = Y, a_i = Y, u = N)(s1),
            to(s0, valid = Y, addDone = Y, a_i = N, u = Y)(s2),
            to(s0, valid = Y, addDone = Y, a_i = Y, u = Y)(s3),
            to(s0, valid = Y, addDone = Y, a_i = N, u = N)(s7),
            to(s1, addDone = Y)(s4),
            to(s1, addDone = N)(s1),
            to(s2, addDone = Y)(s4),
            to(s2, addDone = N)(s2),
            to(s3, addDone = Y)(s4),
            to(s3, addDone = N)(s3),
            to(s7, addDone = Y)(s4),
            to(s7, addDone = N)(s7),
            to(s4, iBreak = Y, addSign = N)(s5),
            to(s4, iBreak = Y, addSign = Y)(s6),
            to(s4, iHead = N, iBreak = N, a_i = Y, u = N)(s1),
            to(s4, iHead = N, iBreak = N, a_i = N, u = Y)(s2),
            to(s4, iHead = N, iBreak = N, a_i = Y, u = Y)(s3),
            to(s4, iHead = N, iBreak = N, a_i = N, u = N)(s7),
            to(s5, addDone = Y)(s6),
            to(s5, addDone = N)(s5),
            "????????"
          ).mkString("\n")
        )
      }
    )
    .asTypeOf(StateType.Type())

  i := Mux1H(
    Map(
      state.asUInt()(0) -> 1.U,
      state.asUInt()(4) -> i.rotateLeft(1),
      (state.asUInt & "b11101110".U).orR -> i
    )
  )

  b_add_p := Mux(addDone & state.asUInt()(0), debounceAdd, b_add_p)

  u := Mux1H(
    Map(
      state.asUInt()(0) -> (a(0).asUInt & b(0).asUInt & pPrime.asUInt),
      (state.asUInt & "b10001110".U).orR -> ((add_stable(1) + (((a & (i.rotateLeft(1))).orR) & b(0))) & pPrime.asUInt),
      (state.asUInt & "b01110000".U).orR -> u
    )
  )

  a_i := Mux1H(
    Map(
      state.asUInt()(0) -> a(0),
      (state.asUInt & "b10001110".U).orR -> (a & (i.rotateLeft(1))).orR,
      (state.asUInt & "b01110000".U).orR -> a_i
    )
  )

  nextT := Mux1H(
    Map(
      state.asUInt()(0) -> 0.U,
      state.asUInt()(4) -> (add_stable >> 1),
      state.asUInt()(5) -> add_stable,
      (state.asUInt & "b11001110".U).orR -> nextT
    )
  )

  adder.a := Mux1H(
    Map(
      state.asUInt()(0) -> p,
      (state.asUInt & "b11111110".U).orR -> nextT
    )
  )
  adder.b := Mux1H(
    Map(
      (state.asUInt & "b00000011".U).orR -> b,
      state.asUInt()(2) -> p,
      state.asUInt()(3) -> b_add_p,
      state.asUInt()(7) -> 0.U,
      state.asUInt()(5) -> -p
    )
  )
  lazy val debounceAdd = Mux(addDone, adder.z, 0.U)
  add_stable := Mux(addDone, debounceAdd, add_stable)

  // output
  out := nextT
  out_valid := state.asUInt()(6)
}

class DummyAdd(width: Int, pipe: Int) extends Module {
  val valid = IO(Input(Bool()))
  val a = IO(Input(UInt(width.W)))
  val b = IO(Input(UInt(width.W)))
  val z = IO(Output(UInt(width.W)))
  val rs = Seq.fill(pipe + 1) { Wire(chiselTypeOf(z)) }
  rs.zipWithIndex.foreach {
    case (r, i) =>
      if (i == 0) r := Mux(valid, a + b, 0.U) else r := Mux(valid, RegNext(rs(i - 1)), 0.U)
  }
  z := rs.last
}

trait MontgomeryModule extends HasRegMap {
  implicit val p: Parameters
  def params: MontgomeryParams

  // use q for p here since p is used by Parameters
  // to support 4096-bit,  a and b and q should read 32bit per cycle (128 cycles)
  val q = Module(new Queue(UInt(params.width.W), 128))
  val pPrime = Reg(Bool())
  val a = Module(new Queue(UInt(params.width.W), 128))
  val b = Module(new Queue(UInt(params.width.W), 128))
  val input_width1 = Reg(UInt(params.width.W)) // block index 0~255
  val input_width2 = Reg(UInt(16.W)) // shift amount
  val MontOut = Module(new Queue(UInt(params.width.W), 128))
  // 0 for idle, 1 for reset, 2 for ready
  // reset not implemented yet
  val control = RegInit(0.U(2.W))
  val status = Wire(UInt(1.W))

  val block = 128
  val length = 32 * block // 4096
  val impl = Module(new MontgomeryMMIOChiselModule(length, params.addPipe))

  val q4096 = Reg(Vec(block, UInt(32.W)))
  val a4096 = Reg(Vec(block, UInt(32.W)))
  val b4096 = Reg(Vec(block, UInt(32.W)))
  val input_width4096 = Reg(Vec(block*2, UInt(16.W)))
  val valid4096 = RegInit(0.U(1.W))
  impl.p := q4096.asUInt
  impl.pPrime := pPrime
  impl.a := a4096.asUInt
  impl.b := b4096.asUInt
  impl.input_width := input_width4096.asUInt
  impl.valid := valid4096
  
  val inputCounter_a = Counter((control === 2.U) && (a.io.deq.valid), block+1)._1
  val inputCounter_b = Counter((control === 2.U) && (b.io.deq.valid), block+1)._1
  val inputCounter_q = Counter((control === 2.U) && (q.io.deq.valid), block+1)._1
  valid4096 := Mux((inputCounter_a >= block.U) && (inputCounter_b >= block.U) && (inputCounter_q >= block.U), 1.U, valid4096)
  q.io.deq.ready := control === 2.U
  a.io.deq.ready := control === 2.U
  b.io.deq.ready := control === 2.U

  q4096(inputCounter_q) := Mux((control === 2.U) && (q.io.deq.valid), q.io.deq.bits, q4096(inputCounter_q))
  a4096(inputCounter_a) := Mux((control === 2.U) && (a.io.deq.valid), a.io.deq.bits, a4096(inputCounter_a))
  b4096(inputCounter_b) := Mux((control === 2.U) && (b.io.deq.valid), b.io.deq.bits, b4096(inputCounter_b))  
  input_width4096.zipWithIndex.foreach {
    case (row, i) =>
      row := Mux(i.asUInt === input_width1.asUInt, 1.U << input_width2, 0.U)
  }

  val tmpOut = RegInit(0.U(length.W))  
  val tmpStatus = RegInit(0.U(1.W))
  tmpOut := Mux(impl.out_valid, impl.out, tmpOut)
  tmpStatus := Mux(impl.out_valid, 1.U, tmpStatus)
  
  val counter_enable = RegInit(1.U(1.W))
  val outputCounter = Counter((tmpStatus === 1.U) && (counter_enable === 1.U) && (MontOut.io.enq.ready), block+1)._1
  val out32 = RegInit(0.U(length.W))
  counter_enable := Mux(outputCounter >= block.U, 0.U, counter_enable)
  out32 := Mux(
    (outputCounter === 0.U) && (counter_enable === 1.U) && (MontOut.io.enq.ready), 
    tmpOut, 
    Mux((outputCounter === block.U) || (outputCounter === 0.U),
      out32,
      out32 >> 32.U)
  )
  
  // status := Mux((counter_enable === 0.U), tmpStatus, 0.U)
  status := tmpStatus
  val tmpStatusNext = Reg(UInt((1).W))
  tmpStatusNext := tmpStatus
  MontOut.io.enq.bits := Mux((tmpStatusNext === 1.U) && (MontOut.io.enq.ready), out32(31, 0), 1111.U)
  MontOut.io.enq.valid := Mux((tmpStatusNext === 1.U) && (MontOut.io.enq.ready), 1.U, 0.U)

  regmap(
    0x00 -> Seq(
      RegField.r(1, status)), // a read-only register capturing current status
    0x04 -> Seq(
      RegField.w(2, control)),
    0x08 -> Seq(
      RegField.w(1, pPrime)),
    0x0C -> Seq(
      RegField.w(params.width, q.io.enq)), // a plain, write-only register
    0x10 -> Seq(
      RegField.w(params.width, a.io.enq)),
    0x14 -> Seq(
      RegField.w(params.width, b.io.enq)),
    0x18 -> Seq(
      RegField.w(params.width, input_width1)),
    0x1C -> Seq(
      RegField.w(16, input_width2)),
    0x20 -> Seq(
      RegField.r(params.width, MontOut.io.deq)))
}

class MontgomeryTL(params: MontgomeryParams, beatBytes: Int)(implicit p: Parameters)
  extends TLRegisterRouter(
    params.baseAddress, "montgomery", Seq("plct-caat,montgomery"),
    beatBytes = beatBytes)(
      new TLRegBundle(params, _))(
      new TLRegModule(params, _, _) with MontgomeryModule)

class MontgomeryAXI4(params: MontgomeryParams, beatBytes: Int)(implicit p: Parameters)
  extends AXI4RegisterRouter(
    params.baseAddress,
    beatBytes=beatBytes)(
      new AXI4RegBundle(params, _))(
      new AXI4RegModule(params, _, _) with MontgomeryModule)

trait CanHavePeripheryMontgomery { this: BaseSubsystem =>
  private val portName = "montgomery"

  // Only build if we are using the TL (nonAXI4) version
  val montgomery = p(MontgomeryKey) match {
    case Some(params) => {
      if (params.useAXI4) {
        val montgomery = LazyModule(new MontgomeryAXI4(params, pbus.beatBytes)(p))
        pbus.toSlave(Some(portName)) {
          montgomery.node :=
          AXI4Buffer () :=
          TLToAXI4 () :=
          // toVariableWidthSlave doesn't use holdFirstDeny, which TLToAXI4() needsx
          TLFragmenter(pbus.beatBytes, pbus.blockBytes, holdFirstDeny = true)
        }
        Some(montgomery)
      } else {
        val montgomery = LazyModule(new MontgomeryTL(params, pbus.beatBytes)(p))
        pbus.toVariableWidthSlave(Some(portName)) { montgomery.node }
        Some(montgomery)
      }
    }
    case None => None
  }
}

class WithMontgomery(baseAddress: BigInt, useAXI4: Boolean = false, width: Int, addPipe: Int) extends Config((site, here, up) => {
  case MontgomeryKey => Some(MontgomeryParams(baseAddress = baseAddress, useAXI4 = useAXI4, width = width, addPipe = addPipe))
})
