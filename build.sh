rm -rf target
mkdir target
nasm -f bin BIOS.asm -o target/BIOS.ROM
nasm -f bin NETBOOT.asm -o target/NETBOOT.ROM
