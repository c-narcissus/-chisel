
import chisel3._
import chisel3.util._
class FloatingPoint extends Bundle {
  val sign = UInt(1.W)
  val exp = UInt(8.W)
  val significand = UInt(23.W)

  def isZero(): Bool = exp === 0.U && significand === 0.U
  def isInf(): Bool = exp === 255.U && significand === 0.U
  def isNaN(): Bool = exp === 255.U && significand =/= 0.U
}
class SignCalculator extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(1.W))
    val b = Input(UInt(1.W))
    val valid = Input(Bool())
    val out = Output(UInt(1.W))
    val ready = Output(Bool())
  })

  io.out := 0.U
  io.ready := false.B

  when(io.valid) {
    io.out := io.a ^ io.b
    io.ready := true.B
  }
}
class ExpCalculator extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(9.W))
    val b = Input(UInt(9.W))
    val valid = Input(Bool())
    val out = Output(UInt(9.W))
    val ready = Output(Bool())
  })

  io.out := 0.U
  io.ready := false.B

  when(io.valid) {
    val bias = 127.U(9.W)
    io.out := (io.a + bias) - io.b  // 被除数指数 + 偏移 - 除数指数
    io.ready := true.B
  }
}

class SigDivider extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(25.W))
    val b = Input(UInt(25.W))
    val valid = Input(Bool())
    val out = Output(UInt(25.W))
    val ready = Output(Bool())
  })

  io.out := 0.U
  io.ready := false.B
  val extendedA = Wire(UInt(49.W))
  extendedA := io.a<<24
  when(io.valid) {

    io.out := (extendedA / io.b  )
    io.ready := true.B
  }
}

class FDiv extends Module {
  val io = IO(new Bundle {
    val a = Input(new FloatingPoint)
    val b = Input(new FloatingPoint)
    val valid = Input(Bool())
    val ack = Input(Bool())
    val idle = Output(Bool())
    val out = Output(new FloatingPoint)
    val ready = Output(Bool())
  })

  val sWait :: sCompute :: sAdjust :: sNormalize :: sFinish :: Nil = Enum(5)
  val state = RegInit(sWait)

  val signCalc = Module(new SignCalculator)
  val expCalc = Module(new ExpCalculator)
  val sigDiv = Module(new SigDivider)

  val expReg = RegInit(0.U(9.W))
  val divResultReg = RegInit(0.U(24.W))
  // 握手信号连接
  signCalc.io.a := io.a.sign
  signCalc.io.b := io.b.sign
  signCalc.io.valid := (state === sCompute)
  val expCalcA=RegInit(0.U(9.W))
  val expCalcB=RegInit(0.U(9.W))
  expCalc.io.a := expCalcA
  expCalc.io.b := expCalcB
  expCalc.io.valid :=  (state === sCompute)
  val sigDivA=RegInit(0.U(25.W))
  val sigDivB=RegInit(0.U(25.W))
  sigDiv.io.a := sigDivA
  sigDiv.io.b := sigDivB
  sigDiv.io.valid :=  (state === sCompute)


  val signOutReg = RegInit(0.U(1.W))
  val expOutReg = RegInit(0.U(8.W))
  val significandOutReg =  RegInit(0.U(23.W))

  io.out.sign := signOutReg
  io.out.exp := expOutReg
  io.out.significand := significandOutReg
  io.idle := false.B
  io.ready := false.B 
  // 状态机逻辑

  switch(state) {
    is(sWait) {
        signOutReg := 0.U
        expOutReg := 0.U
        significandOutReg := 0.U
        sigDivA :=0.U
        sigDivB :=0.U
        expCalcA :=0.U
        expCalcB :=0.U
        io.idle := true.B
        io.ready := false.B  // 确保在等待状态时，ready信号是低的
        when(io.valid) {  // 当有效输入信号被置高时，转移状态
            io.idle := false.B
            state := sAdjust
        }
    }
    is(sAdjust) {
        // 归一化尾数并确保被除数小于除数

        when(io.a.isNaN || io.b.isNaN || (io.a.isInf && io.b.isZero) || (io.a.isZero && io.b.isInf)) {
            // NaN的情况
            signOutReg := 0.U
            expOutReg := "b11111111".U  // 表示无穷或NaN
            significandOutReg := "b00000000000000000000001".U  // NaN的尾数部分通常设置非零
            state := sFinish
           
        }.elsewhen(io.a.isZero || io.b.isInf) {
            // 输出为零
            signOutReg := io.a.sign ^ io.b.sign
            expOutReg := 0.U
            significandOutReg := 0.U
            state := sFinish

        }.elsewhen(io.a.isInf || io.b.isZero) {
            // 输出为无穷大
            signOutReg := io.a.sign ^ io.b.sign
            expOutReg := "b11111111".U
            significandOutReg := 0.U
            state := sFinish
        
        }.otherwise {
            when(io.a.significand >= io.b.significand) {
                sigDivA :=  0.U(1.W) ## 1.U(1.W) ## io.a.significand
                expCalcA := (0.U(1.W) ## io.a.exp) + 1.U
              
            }.otherwise {
                sigDivA := 1.U(1.W) ##  io.a.significand ## 0.U(1.W) 
                expCalcA :=  0.U(1.W) ## io.a.exp
            
            }
            sigDivB :=1.U(1.W) ##  io.b.significand ## 0.U(1.W) 
            expCalcB :=  0.U(1.W) ## io.b.exp
            state := sCompute

        }

    }

    is(sCompute) {
      
        // 特殊情况处理
         when(signCalc.io.ready && expCalc.io.ready && sigDiv.io.ready) {
            // 没有特殊情况，准备进行常规的除法计算
            signOutReg := signCalc.io.out // Always set the sign
            divResultReg := sigDiv.io.out(24,1)
            expReg := expCalc.io.out
            state := sNormalize  // 转移到尾数调整状态
        
        }
       
    }

    is(sNormalize) {
      
        val normalized = Wire(UInt(24.W))
        val overflow = Wire(Bool())
        val decreasedExp=Wire(UInt(9.W))
        normalized := 0.U
        overflow := false.B

        // 检查是否需要左移规格化（尾数的最高位应为1）
        when(divResultReg(23) === 0.U && divResultReg=/= 0.U) {
            
            normalized := divResultReg<< 1
            decreasedExp :=expReg(8, 0) - 1.U
        }.otherwise {
            normalized := divResultReg
            decreasedExp :=expReg(8, 0) 
        }
        
        // 检查指数是否溢出
        overflow := decreasedExp >= "b11111110".U || decreasedExp <= "b00000001".U
        when(overflow) {

            expOutReg := "b11111111".U  // 溢出设置为无穷大
            significandOutReg := 0.U
        }.otherwise {


            expOutReg:= decreasedExp(7,0)
            significandOutReg:= normalized(22, 0)
           
        }

        state := sFinish
    }
    is(sFinish) {
        io.ready := true.B  // 表明结果已经准备好
        when(io.ack) {  // 等待外部确认
            io.ready := false.B
            state := sWait  // 返回等待状态，准备下一轮计算
        }
    }

  }
}


object FDiv extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new FDiv(), Array("--target-dir", "verilog"))
}





