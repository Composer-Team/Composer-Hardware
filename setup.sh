#!/bin/bash
mkdir -p project
echo "sbt.version=1.3.13" > project/build.properties 

# rocket-chip
git clone -q https://github.com/chipsalliance/rocket-chip.git && cd rocket-chip && git checkout f6e0f0e -q && git submodule update --init -q && cd ..

rcpluglen=$(wc -l rocket-chip/project/plugins.sbt | grep -o "[0-9]*")
if test -e project/plugins.sbt ;
then
	echo "plugins.sbt already exists, not copying rocket-chip plugins..."
else
	touch project/plugins.sbt
	cat rocket-chip/project/plugins.sbt >> project/plugins.sbt
fi

if test $rcpluglen -eq $(wc -l project/plugins.sbt | grep -o "[0-9]*") ;
then
	echo "addSbtPlugin(\"org.jetbrains.scala\" % \"sbt-ide-settings\" % \"1.1.1\")" >> project/plugins.sbt
fi

patch=composerToTL.patch
cp $patch rocket-chip/
cd rocket-chip
git apply composerToTL.patch &> /dev/null
cd ..
export MAKEFLAGS="$MAKEFLAGS -j8"
echo "-------------------------------"
echo "Composer set up is complete. Make sure to add the following lines to your bash profile (.bashrc/.zshrc/...)"
echo "export COMPOSER_HARDWARE_DIR=\$COMPOSER_HARDWARE_DIR:`pwd`"
echo "export XDG_CONFIG_HOME=`pwd`/.config"

echo "You must do this for every separate Composer-Hardware directory you set up!"

# rocket-tools + riscv installation
# Chris notes: You might be asking why all this sed is necessary. For some reason the writers of these scripts
# used git:/ paths which sometimes (when using github) require more permissions that are necessary leading to _really_
# long latency git failures. This should fix it and clone quickly! 
# git submodule update --init rocket-tools
# sed -i "s/git:/https:/" rocket-tools/.gitmodules
# cd rocket-tools
# git submodule update --init riscv-gnu-toolchain
# sed -i "s/git:/https:/" riscv-gnu-toolchain/.gitmodules
#cd riscv-gnu-toolchain
#git submodule update --init riscv-qemu
#cd riscv-qemu
#sed -i -E "s/git:\/\/(.*)\/(.*)\.git/https:\/\/\1.\/\2.git/" .gitmodules
# this one works on Mac systems but it's not clear that any of the other tools actually work on Mac
# sed -i -E "s/git:\/\/\(.*\)\/\(.*\)\.git/https:\/\/\1.\/\2.git/" .gitmodules
#cd ../../
#git submodule update --init --recursive
#config=$HOME/.$(basename "$SHELL")rc
#echo "export RISCV=$(pwd)/build/" >> $config
#source $config
# Another annoyance... If you're running on Arm these repos don't work because some of them were
# created (and use config.guess files) that were created before Arm existed... No matter, just
# update the config.guess files!
#
#curl -o ../config.guess "http://git.savannah.gnu.org/gitweb/?p=config.git;a=blob_plain;f=config.guess;hb=HEAD"
#curl -o ../config.sub "http://git.savannah.gnu.org/gitweb/?p=config.git;a=blob_plain;f=config.sub;hb=HEAD"
#chmod u+x ../config.guess
#chmod u+x ../config.sub
#find . -name "config.guess" | while read line ; do cp ../config.guess $line; done
#find . -name "config.sub" | while read line ; do cp ../config.sub $line; done
#echo "To build RISC-V, run 'source $config', cd to the rocket-tools directory and run ./build.sh"

#SFILE=".$(basename "$SHELL")rc"
#VCS_SETUP=/usr/project/apex/synopsys-setup-bash.sh
#echo "test -f $VCS_SETUP && source $VCS_SETUP" >> $SFILE
#AWS_SETUP=/usr/project/apex/aws-setup-bash.sh
#echo "test -f $AWS_SETUP && source $AWS_SETUP" >> $SFILE
#CL_DIR=`pwd`/aws-tools/hdk/cl/developer_designs/composer
#echo "export CL_DIR=$CL_DIR" >> $SFILE
#echo "export PROJ_DIR=`pwd`" >> $SFILE
