#include <memory>
#include <iostream>
#include <string>
#include <set>
#include <vector>

std::string f(const std::string &s) {
   
    return std::move(s);  // Warning: std::move of the const variable has no effect

    std::string s2;
    f(std::move(s2));  // Warning: passing result of std::move as a const reference argument; no move will actually happen
   
    std::set<std::string> set_s{"string_1", "string_2"};
    std::vector<std::string> v{"string_3", "string_4"};
    std::move(set_s.begin(), set_s.end(), v.begin());
    
   return {};
}

int main() {
    std::set<std::string> set_s{"string_1", "string_2"};
    std::vector<std::string> vec_v{"string_3", "string_4"};
    std::vector<std::string> vec_v2{"string_5", "string_6"};
    std::move(set_s.begin(), set_s.end(), vec_v.begin());
    std::move(vec_v.begin(), vec_v.end(), vec_v2.begin());


    struct S {
    std::string str;
    int i;
    };
    S s = { "Hello, world!\n", 42 };
    S s_other = std::move(s);
    s.str = "Lorem ipsum";
    s.i = 99;
    s.i = 100;
    std::cout << s.str;
    S s_other_other = std::move(s_other);
    s_other.i = 42;

    const std::string c_s_param = "something";
    f(c_s_param);

    int x;
    return std::move(x);
    return std::move(x);// Warning: std::move of the variable of a trivially-copyable type has no effect
    const S* s_other2 = new S{"Hello, world!\n", 42};
    S* s_other3;
    *s_other3 = std::move(*s_other2);
    
}


// PATH=$PATH:/home/koldaniel/llvm/llvm-project/build/bin

// rm -rf move_stats/; ninja -C build clang-tidy && echo "------------------------" && ./build/bin/clang-tidy -checks='-*, performance-move-const-arg, bugprone-use-after-move' -header-filter=.* -system-headers ./test.cc

// rm -rf move_stats/; ninja -C build clang-tidy && echo "------------------------" && ./clang-tools-extra/clang-tidy/tool/run-clang-tidy.py clang-tidy -checks='-*, performance-move-const-arg, bugprone-use-after-move' -header-filter=.*
