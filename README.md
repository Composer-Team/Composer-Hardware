# How to build for simulation

All you need to build for simulation is the verilog files produced by Chisel.
The Composer-Runtime repo will find these files based off your definition of the `COMPOSER_ROOT` environment variable.

```shell
sbt run
```

# How to build for AWS

This assumes that you are on a AWS compute instance with the right AMI.
Within the top-level Composer-Hardware directory, run...

```shell
make
```

You may be asked for which of your drivers to use. Select whichever one you want to produce an FPGA instance for.
This script starts Vivado, which may run for a very long time.
The shortest builds can take on the order of an hour and longer builds may take on the order of a day.
You can see the output stream from vivado from the logs written to 
`Composer-Hardware/vsim/build/scripts/<time_stamp>.nohup.out`.

Once that build process has been completed, you can see the report logs for your build in the aforementioned log file
and within the directory `Composer-Hardware/vsim/build/reports/`.
Assuming your build has succeeded, you can produce an AFI using the following script:

```shell
composer-mv
```

You will be prompted for some information that will allow Composer to identify your builds across AWS instances.
If you build failed due to timing, `composer-mv` will not succeed and will warn you.
Producing the AFI from the Vivado tarball takes a few hours as well, but notably less time than the tarball creation.
Once `composer-mv` has begun running, you can safely stop the AWS instance.
After some time has passed, log onto the FPGA instance.
Run the following command to load your instance and start the Composer FPGA management runtime.

```shell
composer-load.py
```

It will ask you some of the same identifying information from when you ran `composer-mv`.
Make sure you enter the identical information.
It will ask you to select which image to load and notify you of the avilability of these images.
If you haven't waited enough time, the instance will be marked as "pending".
If the instance is available, it will be marked "available".

Once you've loaded the image, you can run your code.
Make sure you've installed the software library.
