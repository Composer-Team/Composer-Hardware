import os

width = {
    #'Q': 8,
    'B': 16,
    'H': 16,
    'S': 32,
    'D': 64
}

lane = 1

#for stage in range(1, 6):
for stage in range(0,3):
    #for format in ['S', 'D']:
    for format in ['B','H','S','D']:
        #for lane in range(1, 2):
            suffix = f'_{format}{lane}l{stage}s'
            os.system(f"cp FPNewBlackbox.sv FPNewBlackbox{suffix}.sv")
            os.system(f"sed -i 's/__FLEN__/{width[format]*lane}/' FPNewBlackbox{suffix}.sv")
            fp32 = int(format == "S")
            os.system(f"sed -i 's/__FP32__/{fp32}/' FPNewBlackbox{suffix}.sv")
            fp64 = int(format == "D")
            os.system(f"sed -i 's/__FP64__/{fp64}/' FPNewBlackbox{suffix}.sv")
            bf16 = int(format == "B")
            os.system(f"sed -i 's/__FP16ALT__/{bf16}/' FPNewBlackbox{suffix}.sv")
            fp16 = int(format == "H")
            os.system(f"sed -i 's/__FP16__/{fp16}/' FPNewBlackbox{suffix}.sv")
            os.system(f"sed -i 's/__STAGES__/{stage}/' FPNewBlackbox{suffix}.sv")
            os.system(f"make SUFFIX={suffix}")
