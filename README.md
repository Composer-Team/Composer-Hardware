# Setup

First of all, do not `--recursive` clone this directory. It **WILL** fail. 
Instead, do a shallow clone and use the setup script to pull submodules. 
```bash
git clone https://github.com/ChrisKjellqvist/Composer-Hardware.git
./setup.sh
```

### Install AWS-FPGA
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

### Hardware Declarations
To deploy a custom core design, several steps are necessary. First, develop a module in Chisel to implement the desired
functionality. For this step, refer to the `composer.templates.TemplateCore` module. Composer exposes a number of
interfaces to simplify reading/writing to DRAM or across AXI interfaces. See 
[template.scala](composer/src/main/scala/composer/templates/template.scala) on line 58-60:
```scala
  val input_M1 = readChannels(0)
  val input_M2 = readChannels(1)
  val output = writeChannels(0)
```
These readChannels are independently operating lines to DRAM (or another memory interface) that a core can use with a
simple `Decoupled` interface as supposed to the _real_ DRAM interface which is much more complicated. How do we 
declare these channels? And how do we specify how many we need or the width of them? More on that later... but for now,
know that all you need to do to define a core that can fit into the Composer is to extend `ComposerCore` like in the
following example:
```scala
class TemplateCore(composerCoreParms: ComposerCoreParms)(implicit p: Parameters)
  extends ComposerCore(composerCoreParms)(p) {

```

### Configuration Declarations

Now that you've created a core design that extends the `ComposerCore` interface, it's time to write a RocketChip-style
configuration that descibes how it integrates into a "System". A `ComposerSystem` is an aggregation of the same type of
cores. This is what a runnable configuration looks like:
```scala
class MyTemplateConfig extends Config(
  new WithTemplate(1) ++
    new WithAWSMem ++
    new WithComposer
)
```
And end-user doesn't need to worry about `WithAWSMem` and `WithComposer`. `WithComposer` tricks the RocketChip systems
into letting us use their components and is necessary for ALL Composer designs. `WithAWSMem` will change per-backend,
but since we only currently support AWS instances, there are no other options. `WithTemplate` is what puts a user
design into the Composer framework. Before we look at how it works, I'll give a quick example of how RocketChip
Configs work. 
```scala
case object ComposerSystemsKey extends Field[Seq[ComposerSystemParams]]
```
`Config` objects use keys to indentify members that may (or may not) be part of the current configuration. If you
noticed above when we defined `TemplateCore`, there is an `(implicit p: Parameters)` as an argument. This is the case
for every module in a RocketChip system with very few exceptions. Using `p`, we can access or change the parameters
in the current context. For instance, we can obtain a list of all of the current `ComposerSystemParams` objects using
the `ComposerSystemsKey` like so:
```scala
val currentSystems = p(ComposerSystemsKey)
```

Now that you know a little bit about Configs and how we access them during run-time, let's see how we initialize them
for the case of a system that uses our `TemplateCore` design.

```scala
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

Above is the `WithTemplate` Config. Don't worry too much about `(site, here, up)`, just know that they allow us to
access parent configs and current config contexts. To add a system that uses our `TemplateCore` design to the Composer,
we need to add a new `ComposerSystemParams` object to the current list of composer systems. We do that by first
taking the existing list of systems (`up(ComposerSystemsKey, site)`), and appending a new `ComposerSystemParams`
object. Here is how we define a new `ComposerSystemsParams`:

```scala
case class ComposerSystemParams(coreParams: ComposerCoreParams,
                                nCores: Int,
                                system_id: Int,
                                buildCore: (ComposerSystemParams, Parameters) => ComposerCore,
                                bufSize: Int = 1024,
                                bufMasked: Boolean = false,
                                doubleBuf: Boolean = false,
                                channelQueueDepth: Int = 32)
```

`coreParams` describes the properties that all Composer cores must have. Right now this is just read/write channel
information. `nCores` is the number of your designs you want in the system. `system_id` is an unfortunate blemish on
this code base. Whenever a command is sent to the composer, it specifies which system it wants to use using a numerical
identifier. 

> As an aside... If this identifier changes in between hardware elaborations, then code that incorporates system IDs into
> the binary will need to be re-compiled on every hardware elaboration, which is not something that should be true.
> System IDs _should_ be stable. But on the other hand, forcing the user to define `system_id` seems unnecessary and
> Composer should be able to abstract this away. Maybe instead of compiling system IDs into the binary, there should be
> some sort of config file output from the Composer and the Composer runtime requires this config file to run. Instead
> of compiling system IDs into the binary then, the binary knows the "name" for each system it wants to use for each
> command and performs the map from "name" to ID using the config file at runtime. If you (Justin or Brendan) are
> interested in implementing this, that could be nice. If this stream-of-consciousness ramble didn't make a lot of
> sense but you want to know more, just ping me. ANYWAYS! Back to `ComposerSystemParams`...

The next member of `ComposerSystemParams` is `buildCore`. This is where we tell the system how to incorporate our
design into the framework. The IO from the system is already defined and so it's easy enough to hook it in, all we
need to do is expose the constructor of our module (and wrap it in the Chisel `Module`).
The rest of the parameters aren't particularly important and you don't need to worry about changing them (I think).

Now you have everything you need to define a hardware module and hook it into the Composer. This is already much
simpler than the previous Composer interface, so any contributions you have to improving it further are much
appreciated.
### Software Declarations

See the [Composer software repository](github.com/ChrisKjellqvist/Composer-Software) for more information.

# Building

To build the verilog sources from Chisel. To build the hardware associated with `MyTemplateConfig`, run the following:
```shell
cd vsim
make verilog CONFIG=MyTemplateConfig
```

Now, the verilog sources corresponding to the accelerator design should be in the generated-src directory found within
vsim. Some additional steps are necessary to preparing these sources for F1 image creation. **NOTE**: this should be
scripted away before release to the public.

Hooking into the AWS top-level module is currently a subject of improvement and currently only supports 1 DDR
interface and has to be adjusted manually...
You should have set the `COMPOSER_AWS_SDK_DIR` as part of the setup procedure.
The [cl_template.sv](https://github.com/aws/aws-fpga/blob/4750aacb4dac9d464b099b27e4337220cf0b0713/hdk/common/shell_v04261818/new_cl_template/design/cl_template.sv)
file from the [aws-fpga](https://github.com/aws/aws-fpga) repo contains the top-level module for whenever we're building
an image for the F1 FPGA.
The Composer Framework provides a [slightly modified version of this module](https://github.com/lisakwu/aws-fpga-genomeacc-simple/blob/3df6ecfd1101105ed0f64ea3c63c8509a4ed3c10/hdk/cl/developer_designs/composer/design/adamacc_aws.sv)
that hooks in our generated code from the previous section.

In our generated code there are a number of code snippets like this:
```scala
// in generated verilog
output [4:0]   axi4_mem_0_aw_bits_id,
```

However if we look at the Amazon-provided code it looks like:
```scala
// in aws provided wrapper
output [15:0] io_mem_0_aw_bits_id,
```

This discrepancy happens for a number of ports (they all end with `...bits_id`. Change all of the number of bits
in the aws-wrapper code to the number of bits found in the generated verilog. 

Instructions on building the F1 image and simulation testbed will be written soon. 

# Tid-bits

