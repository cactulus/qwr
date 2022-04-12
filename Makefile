EXEC := qwr.out
BIN := qwr

SRCS := $(wildcard src/*.cpp)
OBJS := $(SRCS:%.cpp=%.o)
DEPS := $(SRCS:%.cpp=%.d)

LLVM_CONFIG:=llvm-config
CXXFLAGS := -std=c++20 -O3 -DDIAGNOSTICS

CXX = clang++

LIBS := $(shell $(LLVM_CONFIG) --system-libs --ldflags --cxxflags --libs all)

$(BIN): $(OBJS)
	$(CXX) -MMD -MP $(CXXFLAGS) -o $(BIN) $(OBJS) $(LIBS)

clean:
	-$(RM) $(PROG) $(OBJS) $(DEPS) 

-include $(DEPS)
