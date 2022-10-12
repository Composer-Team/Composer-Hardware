# Recommended Setup
As a recommended do not clone this repo. Instead, clone the parent repo: [Composer](https://github.com/ChrisKjellqvist/Composer).
Regardless, we provide installation instructions for this repo as well which may be useful if you are developing for the Composer.

All that is necessary to get Composer-Hardware up and running is clone the repo and run the setup script.
The setup script clones dependencies, patches them, and sets up some environment variables.
The install script may tell you to add some variables to your path after installation so pay attention for those messages.
```bash
git clone https://github.com/ChrisKjellqvist/Composer-Hardware.git
./setup.sh
```

### Backend: AWS-FPGA
Clone the [Amazon AWS FPGA SDK](https://github.com/aws/aws-fpga) alongside the Composer-Hardware (this) and
[Composer-Software](https://github.com/ChrisKjellqvist/Composer-Software) repositories.
```shell
git clone --recursive https://github.com/aws/aws-fpga.git
```
In order for the Composer tools to find the SDK, export the installation path to the `COMPOSER_AWS_SDK_DIR` variable
within your `.bashrc` file.
```shell
echo "export COMPOSER_AWS_SDK_DIR=<path_to_sdk>" >> .bashrc
```

# Developing a Core
Developing a core for the composer is super easy.
It is broken into two parts: the core itself and the system configuration.

### Hardware Declarations
How do we design a core for deployment in Composer?
First, develop a module in Chisel to implement the desired functionality. 
For this step, refer to the [LFSRCore](src/main/scala/design/LFSRCore.scala) module.

Your design should be a `ComposerCore`.
The declaration of a top-level core module should like the following code-block and will not change much at all from design to design. 
```scala
class MyTemplateCore(composerCoreParms: ComposerCoreParms)(implicit p: Parameters)
  extends ComposerCore(composerCoreParms)(p) {
```

The core design needs to drive the IO interface of the core, defined by `ComposerCoreIO` (see [source](composer/src/main/scala/composer/ComposerCore.scala)).
The IO is discussed in more depth in the [Composer-Software](https://github.com/ChrisKjellqvist/Composer-Software).

Besides correctly driving the IO pins and extending `ComposerCore`, that's all it takes to develop a module compatible with the Composer!

[//]: # (Composer exposes a number of interfaces to simplify reading/writing to DRAM or across AXI interfaces. See )

[//]: # ([template.scala]&#40;composer/src/main/scala/composer/templates/template.scala&#41; on line 58-60:)

[//]: # (```scala)

[//]: # (val input_M1 = readChannels&#40;0&#41;)

[//]: # (val input_M2 = readChannels&#40;1&#41;)

[//]: # (val output = writeChannels&#40;0&#41;)

[//]: # (```)

[//]: # (These readChannels are independently operating lines to DRAM &#40;or another memory interface&#41; that a core can use with a)

[//]: # (simple `Decoupled` interface as supposed to the _real_ DRAM interface which is much more complicated. How do we )

[//]: # (declare these channels? And how do we specify how many we need or the width of them? More on that later... but for now,)

[//]: # (know that all you need to do to define a core that can fit into the Composer is to extend `ComposerCore` like in the)

[//]: # (following example:)

### Configuration Declarations
But how do we take this design and integrate it into a larger system? We provide a configuration!
Let's discuss some terminology first.
A Composer core is an unique instantiation of the `ComposerCore` class.
A Composer system is a collection of Composer cores of the same specialization. 
For instance, the `TemplateCore` above is a specialization that implements some user function on top of the existing `ComposerCore` framework.
If you were developing a linear algebra accelerator you might have a `ComposerCore` definition for a sparse matrix multiply and a separate definition for a dense matrix multiply.
You would have a system containing multiple sparse matrix multiply cores and likewise a system containing multiple dense matrix multiply cores.

Since we've now defined a core, we need to tell Composer how to build a system of them.
A top-level configuration will look something like this.

```scala
class MyTemplateConfig extends Config(
  new WithTemplate(1) ++
    new WithAWSMem ++
    new WithComposer
)

class WithTemplate(withNCores: Int) extends Config((site, here, up) => {
  case ComposerSystemsKey => up(ComposerSystemsKey, site) ++ Seq(ComposerSystemParams(
    coreParams = ComposerCoreParams(
      nMemXacts = 1,
      readChannelParams = Seq.fill(2)(ComposerChannelParams()),
      writeChannelParams = Seq(ComposerChannelParams())
    ),
    nCores = withNCores,
    system_id = 0,
    buildCore = { case (systemParams: ComposerSystemParams, parameters: Parameters) =>
      Module(new TemplateCore(systemParams.coreParams)(parameters))
    }))

  case TemplateCoreParamKey => TemplateConfig(8, 8)
})
```

First, let's look at `WithTemplate(withNCores: Int)`.
This is a configuration fragment that defines a system of `withNCores` `TemplateCore`s.
The first thing we see is `case ComposerSystemsKey => ...`. 
Composer borrows Rocket-Chip's parameter framework (among other things) and uses it to help parameterize the system.
`ComposerSystemsKey` is defined below.
```scala
case object ComposerSystemsKey extends Field[Seq[ComposerSystemParams]]
````

This object is used to store and read the parameters for all the systems in the system.
This type description means that whenever we access the `CompsoerSystemsKey` `Field` within the parameter object, we will get a `Seq[ComposerSystemParams]`.
You can see `(implicit p: Parameters)` above in the definition of `TemplateCore`.

Within our case statement above, we're saying that `WithTemplate` takes the existing definition of `ComposerSystemsKey` (by accessing `up`),
and appends a new `ComposerSystemParams` to the end of the sequence.
The `ComposerSystemParams` contains the parameters for a single system.
The `coreParams` field is a `ComposerCoreParams` object that contains the parameters for a single core.
The `nCores` field is the number of cores in the system.
The `system_id` field is a unique identifier for the system used within the Composer instruction format.
The `buildCore` field is a function that takes the system parameters and the global parameters and returns a `Module` that implements the core (in short, it exposes your module's constructor).
You can see an example above where we create a new `TemplateCore` within the `buildCore` lambda.
Nothing complicated needs to happen there.

Within `ComposerCoreParams` we have several fields.
The `nMemXacts` field is the number of memory transactions that can be outstanding at any given time.
The `readChannelParams` and `writeChannelParams` fields are the parameters for the read and write channels, respectively.
For this example, our core has two read channels and one write channel, though your core may have a different parameterization.
In the `LFSRCore` example within the `src` directory, there are no read or write channels.

Now that we've discussed how to write the complicated part of the config, we need to make a final top-level config:

```scala
class MyTemplateConfig extends Config(
  new WithTemplate(1) ++
    new WithAWSMem ++
    new WithComposer
)
```

We need only use the config we defined above and concatenate on `WithAWSMem` and `WithComposer`.
These are system-defined configs that define the memory system and various things necessary to tricking Rocket-Chip into letting us use their componenets.
`WithAWSMem` allows us to parameterize for the AWS backend so this may change when support more backends.


[//]: # (> As an aside... If this identifier changes in between hardware elaborations, then code that incorporates system IDs into)

[//]: # (> the binary will need to be re-compiled on every hardware elaboration, which is not something that should be true.)

[//]: # (> System IDs _should_ be stable. But on the other hand, forcing the user to define `system_id` seems unnecessary and)

[//]: # (> Composer should be able to abstract this away. Maybe instead of compiling system IDs into the binary, there should be)

[//]: # (> some sort of config file output from the Composer and the Composer runtime requires this config file to run. Instead)

[//]: # (> of compiling system IDs into the binary then, the binary knows the "name" for each system it wants to use for each)

[//]: # (> command and performs the map from "name" to ID using the config file at runtime. If you &#40;Justin or Brendan&#41; are)

[//]: # (> interested in implementing this, that could be nice. If this stream-of-consciousness ramble didn't make a lot of)

[//]: # (> sense but you want to know more, just ping me. ANYWAYS! Back to `ComposerSystemParams`...)


Now you have everything you need to define a hardware module and hook it into the Composer. This is already much
simpler than the previous Composer interface, so any contributions you have to improving it further are much
appreciated.
### Software Declarations

See the [Composer software repository](github.com/ChrisKjellqvist/Composer-Software) for more information.

# Building

To build the verilog sources from Chisel. To build the hardware associated with `MyTemplateConfig`, run the following:
```shell
cd vsim
make verilog CONFIG=<package>.<my-top-level-config>
```

Now, the verilog sources corresponding to the accelerator design should be in the generated-src directory found within
vsim. Some additional steps are necessary to preparing these sources for F1 image creation. **NOTE**: this should be
scripted away before release to the public.

# Simulation

Once you've built the verilog sources, you can simulate them using the following command:
```shell
composer-make
verilator/Composer_Verilator 
```
It's as simple as that! To simulate though, you'll need to write a user-program to simulate it, but you should look at the [Composer-Software](https://github.com/ChrisKjellqvist/Composer-Software) repo for that.


[//]: # (Hooking into the AWS top-level module is currently a subject of improvement and currently only supports 1 DDR)

[//]: # (interface and has to be adjusted manually...)

[//]: # (You should have set the `COMPOSER_AWS_SDK_DIR` as part of the setup procedure.)

[//]: # (The [cl_template.sv]&#40;https://github.com/aws/aws-fpga/blob/4750aacb4dac9d464b099b27e4337220cf0b0713/hdk/common/shell_v04261818/new_cl_template/design/cl_template.sv&#41;)

[//]: # (file from the [aws-fpga]&#40;https://github.com/aws/aws-fpga&#41; repo contains the top-level module for whenever we're building)

[//]: # (an image for the F1 FPGA.)

[//]: # (The Composer Framework provides a [slightly modified version of this module]&#40;https://github.com/lisakwu/aws-fpga-genomeacc-simple/blob/3df6ecfd1101105ed0f64ea3c63c8509a4ed3c10/hdk/cl/developer_designs/composer/design/adamacc_aws.sv&#41;)

[//]: # (that hooks in our generated code from the previous section.)

[//]: # ()
[//]: # (In our generated code there are a number of code snippets like this:)

[//]: # (```scala)

[//]: # (// in generated verilog)

[//]: # (output [4:0]   axi4_mem_0_aw_bits_id,)

[//]: # (```)

[//]: # ()
[//]: # (However if we look at the Amazon-provided code it looks like:)

[//]: # (```scala)

[//]: # (// in aws provided wrapper)

[//]: # (output [15:0] io_mem_0_aw_bits_id,)

[//]: # (```)

[//]: # ()
[//]: # (This discrepancy happens for a number of ports &#40;they all end with `...bits_id`. Change all of the number of bits)

[//]: # (in the aws-wrapper code to the number of bits found in the generated verilog. )

[//]: # ()
[//]: # (Instructions on building the F1 image and simulation testbed will be written soon. )

[//]: # (# Tid-bits)

