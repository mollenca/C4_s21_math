CC := gcc
CFLAGS := -Wall -Werror -Wextra -std=c11
GCOVFLAGS := -L. --coverage  -ls21_math
OS = $(shell uname)
ifeq ($(OS), Linux)
	CHECK_FLAGS = -lcheck -lm -pthread -lrt -lsubunit
else
	CHECK_FLAGS = -lcheck
endif

.PHONY: all clean rebuild check gcov_report test

all: s21_math.a gcov_report

s21_math.a: s21_math.o
	ar cr libs21_math.a s21_math.o
	ranlib libs21_math.a
	cp libs21_math.a s21_math.a

s21_math.o: s21_math.c s21_math.h
	$(CC) $(CFLAGS) -c s21_math.c s21_math.h

test.o: s21_math.h
	$(CC) $(CFLAGS) -c test.c s21_math.h

test: test.o s21_math.a
	$(CC) $(CFLAGS) $(CHECK_FLAGS) test.o -L./ -ls21_math -o $@

gcov_report: test
	$(CC) $(CFLAGS) s21_math.c test.c libs21_math.a $(CHECK_FLAGS) $(GCOVFLAGS) -o $@
	./gcov_report
	lcov -t gcov_report -o rep.info -c -d .
	genhtml -o report rep.info
	open ./report/index.html
	rm -rf *.gcda *.gcn0 *.info

check:
	cp ../materials/linters/.clang-format .
	clang-format -n *.c *.h
	make test
	CK_FORK=no leaks --atExit -- ./test
	rm .clang-format

clean:
	rm -rf *.a *.o *.gcno *.gcda *.info test_output gcov_report report test

rebuild: clean all
