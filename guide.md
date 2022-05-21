# Scala Code
* The code generated from the Scala code will generate the verilog that will run on the FPGA
* A template scala file is included in **adamacc/src/main/scala/mach/template.scala**
1. The package name at the top should be changed to the folder the file is placed under. For example, the template file is placed in the folder mach, so its package name is 
 ```
  package adamacc.mach
 ```
 2. The name of the file is "Template" so every instance of template in the file should be changed to the name of the file. For example, if the new file has the name "Gemm"
 ```
 TemplateCoreParams -> GemmcoreParams
 ```
 
#### Core Parmas
* Variables declared in the coreParams section correspond to the information that has to be included in the Config file
* All the variables **except** inputBytes and outputBytes are the same across all the files. 
	* inputBytes and outputBytes are helpful variables passed into the scala code such that we can maintain modularity in our code during run-time
		* inputBytes = max number of bytes in the input 
		* outputBytes = max number of bytes in the output
	* Any new variables added to this section has to be updated in the Config file too (See the CONFIG file section for more clarification)
	
#### Cores Reponse and Core Request
* These two functions help the accelerator core and the CPU to communicate with each other
* Any value that is sent through a command should be declared in the CoreRequest with its name and size. For example, when sending the immediate value that has a max size of 8 bytes, the corresponding declaration would be 
``` 
class TemplateCoreRequest()(implicit p: Parameters) extends ModularCoreBundle()(p) {
    val imm = UInt(8.W)
    override def cloneType = new  TemplateCoreRequest().asInstanceOf[this.type]
}
```
* CoreResponse is any information that you want to send to the CPU to let it know whether all the operations completed with no errors. For example, if I wanted to send an error code, I would declare
```
class TemplateCoreResponse()(implicit p: Parameters) extends ModularCoreBundle()(p){
    val error = UInt(32.W)
    override def cloneType = new TemplateCoreResponse().asInstanceOf[this.type]
}
```

#### Core IO
* This function just bundles the coreRequest and coreResponse as an IO

#### System
This function sets up the system to interact with host. 
1. Sets up the module by setting the IO and the core. The line of code that starts with "val cores = ..." refers to instantiate an array of the TemplateCore so that we can utilize multiple cores where nCores is the number of cores we used.
```
lazy val module = new ModularSystemImp(this) {
    val io = IO(new ModularSystemIO)
    val cores = Seq.fill(nCores) { Module(new TemplateCore(addressBits, systemParams.coreParams)(p)) 
}
```
2. Call on the setupCode function to connect the cores to memory.
```
setupCode()
```
**NOTE: steps 3-5 are more advanced and typically not used unless you need to send specific commands through a request. Everything in the TemplateSystem function in the template file is all that needs to be there for most programs.**

3. If you need to pass data through a request command (as oppose to through the Config file), you will have to define a function name associated with it and declare it as part of a LocalEnum, where the integer that is passed into the LocalEnum is number_of_function_declared + 2 (other 2 is defined elsewhere dealing with the read and write lengths). 
```
val funct_len :: Nil = LocalEnum(5)
```
4. Get the data by declaring a vector register to hold the value (it is a vector to acoomodate for that fact that there are multiple cores) and writing a switch statement to know what values corresponds to what function/command. Each command can send two data with it, with the first data stored in cmd.bits.rs1 and the second transmitted in cmd.bits.rs2. The varibale coreSelect in this case refers to which core we are sending the value to
```
val length = Reg(Vec(nCores, UInt(10.W)))
when (cmd.fire()) {
    switch (funct) {
       is (funct_len) {
          length(coreSelect) := cmd.bits.rs1
       }
    }   
 }   

```
5. Set the appropriate variable for each of the core with the value obtained
```
cores.zipWithIndex.foreach { case(core, i) =>
      core.io.req.bits.inputLen := length(i)
}   

```

6. Set the readLengths and writeLengths for each core 
```
cores.zipWithIndex.foreach { case(core, i) =>
      readLengths(i).foreach(_ := RegNext(cmd.bits.rs2))
      writeLengths(i).foreach(_ := RegNext(cmd.bits.rs2))
}   
```

7. Collect the reponses from each core where resp.bits.data correspond to the id of the core and any variable that is declared in the TemplateCoreResponse
```
    val arbiter = Module(new RRArbiter(new TemplateCoreResponse(), nCores))
    arbiter.io.in <> cores.map(_.io.resp)
    resp.valid := arbiter.io.out.valid
    resp.bits.rd := arbiter.io.out.bits.rd
    resp.bits.data := Cat(arbiter.io.out.bits.id, arbiter.io.out.bits.error.asTypeOf(UInt(32.W)))
    arbiter.io.out.ready := resp.ready

```

#### Core 
This is where the main code goes. 
1. Declare the IO 
```
val io = IO(new TemplateCoreIO(modularInterface)(p)) 
```
2. Retrieve the information passed through coreParams. ModularInterface refers to the superclass the the coreParams is derived from.
```
val inputBytes = modularInterface.inputBytes
val outputBytes = modularInterface.outputBytes
```  
3. Define the read and write channels. For every input, call readChannel to open up a channel. The number that is passed as an argument refers to the index of the channel that is open. Go in incremental order starting at 0. The same thing applies to the write channels. The indexing for read and write channels are separate here so the first write channel will have an index of 0. 
```
val input_M1 = readChannels(0)
val input_M2 = readChannels(1)
val output = writeChannels(0)
```
4. Declare the caches for response and requests to queue up the request and responses.
``` 
val reqCache = Reg(new TemplateCoreRequest())
val respCache = Reg(new TemplateCoreResponse())
```   
5. Declare the states in the state diagram design and initialize to the starting state. The name of the states are declared as a enum for readability purposes. 
```
val s_idle :: s_read :: s_calc_element :: s_calc_row ::  s_write :: s_finish :: Nil = Enum(6)
val state = RegInit(init = s_idle)
```

6. Declare any other registers or control variables needed. For example, if  a buffer is needed to store to inputs (used for value reuse as the read Channels transfers inputs as a stream), the associated declaration is
```
val templateInReg_m1 = RegInit(VecInit(Seq.fill(total_elem)(0.U((outputBytes*8).W))))
```
 In this example, the buffer has a size of total_elem and each element is outputBytes * 8 width.
 
 7. Initialize the io.request, io.response, read channels, and write channels

	a. Set the ready bit of the io.request so no requests is received during setup
	```
	 io.req.ready := false.B
	```
	b. Set the stop variable of the read channels to false. The stop variable is set to true only when the inputs from the read channels are no longer needed. The ready bit is also set to false to prevent inputs from being sent through the read channels during set up.
	```
	 input_M1.stop := false.B
	 input_M1.data.ready := false.B
	```
	c. Do the similar thing for the write channel. Set the valid and finished variable of the write channel to false (Finish is asserted when the data has flowed through the channel). Similarly, set the data.bits variable to 0.U.
	```
	 output.data.valid := false.B
	 output.data.bits := 0.U
	 output.finished := false.B
	```
	 d. Set the id, rd, and error variables of the io.response to correspond to that of the response Cache it is assigned to. Additionally, set the valid bit of the io.response bit to false
	```
	  io.resp.bits.id := respCache.id
	  io.resp.bits.rd := respCache.rd
	  io.resp.bits.error := respCache.error
	  io.resp.valid := false.B
	```
	e. Set the busy variable of io. In the template file, the io is busy when it is in the middle calculation so it is set to 
	```
	io.busy := state =/= s_idle
	```
8. In the idle state, set the ready variable of the io.req to start processing requests. io.req.fire() will return true when the system receives a request from the CPU. Once we receive a request, store the request into the cache along with its identifiers and initialize any other variables/registers needed.
```
when (state === s_idle) {
    io.req.ready := true.B
    when (io.req.fire()) {
      reqCache := io.req.bits
      respCache.id := io.req.bits.id
      respCache.rd := io.req.bits.rd
      respCache.error := 0.U
      
      //Initalize any other register/variables
    }

  }
```

9. In the template file example, s_read is the state in which the inputs are read. To read in inputs, set the ready variable of the read channel to true to signify that the program is ready to receive inputs from the read channel. The data.fire() variable of the read channel returns true when there is a value to read from the read channel. The value on the read channel is contained in the data.bits variable. The .asTypeOf() function casts the input value from the read channel into the same type and size as the elements of templateInReg. 
```
when (state === s_read) {
    input_M1.data.ready := true.B //ready to receive inputs from read channel
    when (input_M1.data.fire()) { //returns true when there is a value to read from the read channel
      templateInReg_m1(readIdx_m1) := (input_M1.data.bits).asTypeOf(templateInReg_m1(0)) //read into buffer

      readIdx_m1 := readIdx_m1 + 1.U
      
      //transition when all the elements are read
      when(readIdx_m1 === (total_elem.U) - 1.U){
        state := s_write 
      }
    }
  }
```
10. When writing out the calculations back to the CPU, refer to the s_write state in the template file example. In this state, the data in the write channel is valid as long as the stop variable is not set. In this example, elem is the value that we want to write out to the write channel. This is accomplished by setting the data.bits variable of the write channel to the elem after elem is casted to the right type and size. The data.fire() function returns true when it has successfully write out the elem value in the write channels.
```
  when (state === s_write){
    output.data.valid := !output.stop
    output.data.bits := elem.asTypeOf(output.data.bits)
    when(output.data.fire()) {
      // Write what happens after it writes out a data: update writeIdx, go to s_finish, etc
    } 
  }
```
11. Once all the data is written out, set the stop, finished, and valid bits of the read channels, write channels, and io.response, repectively, to let the system know to stop reading inputs and sending outputs. The io.resp.fire() function returns true when the response has been successfully sent to the CPU.
```
when(state === s_finish) {
    input_M1.stop := true.B
    input_M2.stop := true.B
    output.finished := true.B
    io.resp.valid := true.B
    when (io.resp.fire()) {
      state := s_idle
    }
 }
```
# Configs File
The configs file is located in genie/src/main/scala/adamaccaws/Configs.scala
1. Each type of accelerator core requires a config function that starts with "With<Name_of_Accelerator>" in order to set things up correctly. In this case, we named our function WithTemplate. The input nCores refer to the number of cores that will run this instance. An overview of the function looks like this: 
```
class WithTemplate(nCores: Int) extends Config((site, here, up) => {
  case ModularSystemsKey => up(ModularSystemsKey, site) ++ Seq(TemplateConfig(
    coreParams  = TemplateCoreParams(
      nMemXacts = 1, 
      readElementSizes = Seq(8, 8), //read sizes in bytes
      writeElementSizes = Seq(8), //writes sizes in bytes
      readLocations = Some(Seq("Mem", "Mem")), 
      writeLocations = Some(Seq("Mem")), 
      inputBytes = 8,
      outputBytes = 4
    ),
    nCores = nCores, 
    opcode = 20
    ))
})
```
Going into each line individually:

   a. Core Params refers to the core params that you defined in your scala file (in this case, it is "TemplateCoreParams"). Make sure all the elements declared under the core Params here are also declared in core params under the associated scala file. Each element is separated by a comma 
  
```
coreParams  = TemplateCoreParams(
	...
),
```
	
   b. nMemXacts: will always be set to one
   
```
nMemXacts = 1,
```
   
   c. readElementSizes: A sequences stating the size of each input (in bytes). The number of elements in the sequence correspond to the number of inputs sent. In this case, template has two inputs where each of its elements are 8 bytes wide. 
   
```
readElementSizes = Seq(8, 8), //read sizes in bytes
```
   
   d. writeElementSizes: Same thing as readElementSizes but for outputs
   
```
writeElementSizes = Seq(8), //writes sizes in bytes
```
   
   e. readLocations: Refers to where to read the inputs from. Default is Mem (meaning it reads from memory). Other options include "Type/Local/Remote". The number of elements in the sequence correspond to the number of inputs.

```
readLocations = Some(Seq("Mem", "Mem"))
```
   
   f. writeLocations: Same thing as readLocations but with outputs
   
```
writeLocations = Some(Seq("Mem"))
```
 **All the variables above are needed for things to run correctly. The other variables that follows in the CoreParams field are optional and determined by the programmer to help with run-time parameterization (such as determining the size of a variable at run time**
   
   g. **Optional** inputBytes/outputbytes: optional variables used to pass information to configure length of input buffers.
   
```
inputBytes = 8,
outputBytes = 4
```
   
   
   h. Set ncores and the opcode. ncores is set to the parameter that is passed into the function. Opcode refers to the unique identifier for this accelerator instance type. Make sure the base value associated with this accerelator type in rocc.h file of the aws-fpga-genomeacc repo is (opcode << 3) or opcode * 8.
   
```
nCores = nCores, 
opcode = 20
```

2. Write a function that instantiate the config function above along with other functions that configure the memory space. WithTemplate takes in the number of cores to run this instance on. The other three functions are predefined to make sure things run correctly. 

```
class TemplateConfig extends Config(
  new WithTemplate(1) ++
    new WithAWSMem ++
    new WithAdamAcc ++
    new BaseCoreplexConfig 
)
```
 
