CXXFLAGS=-std=c++20 -O3 -D_GLIBCXX_DEBUG -Wall -Wextra -DNDEBUG
LIBS=

.PHONY: clean run

sources := $(wildcard *.cpp)
objects := $(patsubst %.cpp,%.o,$(sources))
target := raytracer

$(target): $(objects)
	$(CXX) -o $(target) $(objects) $(LIBS)

%.o: %.cpp
	$(CXX) -c $(CXXFLAGS) $<

%.d: %.cpp
	@set -e; \
	rm -f $@; \
	$(CXX) -MM $(CXXFLAGS) $< >$@.$$$$; \
	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$

-include $(sources:.cpp=.d)

clean:
	rm -f *.o *.d *.d.* $(target)

run: $(target)
	./$(target)
