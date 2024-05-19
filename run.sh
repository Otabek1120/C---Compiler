make clean
make compile

# ./compile --chk_decl --gen_code --print_ast <test1.txt >testout.txt
./compile --chk_decl --gen_code  --print_ast <test1.txt >&testout.txt
make clean