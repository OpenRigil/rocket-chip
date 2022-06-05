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
  // currently the max value is 31
  width: Int = 31,
  addPipe: Int = 1)

case object MontgomeryKey extends Field[Option[MontgomeryParams]](None)

trait HasMontgomeryIO extends BaseModule {
  val pWidth: Int

  val p = IO(Input(UInt(pWidth.W)))
  val pPrime = IO(Input(Bool()))
  val a = IO(Input(UInt(pWidth.W)))
  val b = IO(Input(UInt(pWidth.W)))
  val b_add_p = IO(Input(UInt((pWidth + 1).W))) // b + p
  val valid = IO(Input(Bool())) // input valid
  val out = IO(Output(UInt(pWidth.W)))
  val out_valid = IO(Output(Bool())) // output valid
}

class MontgomeryMMIOChiselModule(val pWidth: Int, addPipe: Int) extends Module
  with HasMontgomeryIO
{
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
  val isAdd = (state.asUInt & "b10101110".U).orR
  adder.valid := isAdd
  val addDoneNext = RegInit(false.B)
  addDoneNext := addDone
  lazy val addDone = if (addPipe != 0) Counter(isAdd && (~addDoneNext), addPipe + 1)._2 else true.B
  val addSign = ((add_stable >> 1) < p.asUInt)
  val a_i = Reg(Bool())
  state := chisel3.util.experimental.decode
    .decoder(
      state.asUInt() ## addDoneNext ## valid ## i.head(1) ## addSign ## u ## a_i, {
        val Y = "1"
        val N = "0"
        val DC = "?"
        def to(
          stateI:  String,
          addDone: String = DC,
          valid:   String = DC,
          iHead:   String = DC,
          addSign: String = DC,
          u:       String = DC,
          a_i:     String = DC
        )(stateO:  String
        ) = s"$stateI$addDone$valid$iHead$addSign$u$a_i->$stateO"
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
            to(s0, valid = Y, a_i = Y, u = N)(s1),
            to(s0, valid = Y, a_i = N, u = Y)(s2),
            to(s0, valid = Y, a_i = Y, u = Y)(s3),
            to(s0, valid = Y, a_i = N, u = N)(s7),
            to(s1, addDone = Y)(s4),
            to(s1, addDone = N)(s1),
            to(s2, addDone = Y)(s4),
            to(s2, addDone = N)(s2),
            to(s3, addDone = Y)(s4),
            to(s3, addDone = N)(s3),
            to(s7, addDone = Y)(s4),
            to(s7, addDone = N)(s7),
            to(s4, iHead = Y, addSign = N)(s5),
            to(s4, iHead = Y, addSign = Y)(s6),
            to(s4, iHead = N, a_i = Y, u = N)(s1),
            to(s4, iHead = N, a_i = N, u = Y)(s2),
            to(s4, iHead = N, a_i = Y, u = Y)(s3),
            to(s4, iHead = N, a_i = N, u = N)(s7),
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

  adder.a := nextT
  adder.b := Mux1H(
    Map(
      state.asUInt()(1) -> b,
      state.asUInt()(2) -> p,
      state.asUInt()(3) -> b_add_p,
      state.asUInt()(7) -> 0.U,
      state.asUInt()(5) -> -p
    )
  )
  val debounceAdd = Mux(addDone, adder.z, 0.U)
  when(addDone)(add_stable := debounceAdd)

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
  val q = Reg(UInt(params.width.W))
  val pPrime = Reg(Bool())
  val a = Reg(UInt(params.width.W))
  val b = Reg(UInt(params.width.W))
  val b_add_p = Reg(UInt((params.width + 1).W))
  val out = Wire(UInt(params.width.W))
  // 0 for idle, 1 for reset, 2 for ready
  // reset not implemented yet
  val control = RegInit(0.U(2.W))
  val status = Wire(UInt(1.W))

  val impl = Module(new MontgomeryMMIOChiselModule(params.width, params.addPipe))

  impl.p := q
  impl.pPrime := pPrime
  impl.a := a
  impl.b := b
  impl.b_add_p := b_add_p
  impl.valid := control === 2.U

  out := impl.out
  status := impl.out_valid

  regmap(
    0x00 -> Seq(
      RegField.r(1, status)), // a read-only register capturing current status
    0x04 -> Seq(
      RegField.w(2, control)),
    0x08 -> Seq(
      RegField.w(1, pPrime)),
    0x0C -> Seq(
      RegField.w(params.width, q)), // a plain, write-only register
    0x10 -> Seq(
      RegField.w(params.width, a)),
    0x14 -> Seq(
      RegField.w(params.width, b)),
    0x18 -> Seq(
      RegField.w(params.width + 1, b_add_p)),
    0x1C -> Seq(
      RegField.r(256, out)))
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
