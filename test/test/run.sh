!# /bin/sh
cd ../src; make; cd -;
run_test -pa ../ebin -include ../src/ -dir $PWD -cover config/cover.conf -logdir $PWD/log;
rm *.beam;
cd ../src; make clean

