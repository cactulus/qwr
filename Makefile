EXEC := qwr.out
BIN := qwr

INSTALL_PATH := /usr/local/bin
STDLIB_PATH := /usr/local/bin/qwrstd/

SRCS := $(wildcard src/*.cpp)
OBJS := $(SRCS:%.cpp=%.o)
DEPS := $(SRCS:%.cpp=%.d)

LLVM_CONFIG:=llvm-config
CXXFLAGS := -std=c++20 -O3 $(shell $(LLVM_CONFIG) --cxxflags)

CXX = clang++

LIBS := $(shell $(LLVM_CONFIG) --system-libs --ldflags --cxxflags --libs all)

$(BIN): $(OBJS)
	$(CXX) -MMD -MP $(CXXFLAGS) -o $(BIN) $(OBJS) $(LIBS)

install: $(BIN)
	mkdir -p $(STDLIB_PATH)
	cp -f -r std/* $(STDLIB_PATH)
	cp -f $(BIN) $(INSTALL_PATH)
	chmod 755 $(INSTALL_PATH)/$(BIN)

uninstall:
	rm -f -r $(STDLIB_PATH)*
	rm -f $(INSTALL_PATH)/$(BIN)

clean: uninstall
	-$(RM) $(PROG) $(OBJS) $(DEPS) 

-include $(DEPS)
