!# /bin/sh
cd ../src; make; cd -;
run_test -pa ../ebin -include ../src/ -config config/split.cfg -dir $PWD -repeat ${1-1} -cover config/cover.conf -logdir $PWD/log;
rm *.beam;
cd ../src; make clean

