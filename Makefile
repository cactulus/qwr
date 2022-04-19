EXEC := qwr.out
BIN := qwr

INSTALL_PATH := /usr/local/bin

SRCS := $(wildcard src/*.cpp)
OBJS := $(SRCS:%.cpp=%.o)
DEPS := $(SRCS:%.cpp=%.d)

LLVM_CONFIG:=llvm-config
CXXFLAGS := -std=c++20 -ggdb -DDIAGNOSTICS

CXX = clang++

LIBS := $(shell $(LLVM_CONFIG) --system-libs --ldflags --cxxflags --libs all)

$(BIN): $(OBJS)
	$(CXX) -MMD -MP $(CXXFLAGS) -o $(BIN) $(OBJS) $(LIBS)


install: $(BIN)
	cp -f $(BIN) $(INSTALL_PATH)
	chmod 755 $(INSTALL_PATH)/$(BIN)

uninstall:
	rm -f $(INSTALL_PATH)/$(BIN)

clean:
	-$(RM) $(PROG) $(OBJS) $(DEPS) 

-include $(DEPS)
