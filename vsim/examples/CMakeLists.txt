cmake_minimum_required(VERSION 3.10)
project(LFSRTest)

set(CMAKE_CXX_STANDARD 17)
find_package(composer REQUIRED)
include_directories($ENV{COMPOSER_ROOT}/Composer-Hardware/vsim/generated-src)
add_executable(allo allocate_test.cc)
target_link_libraries(allo PUBLIC APEX::composer)

add_executable(LFSRTest lfsrtest.cpp)
target_link_libraries(LFSRTest PUBLIC APEX::composer)

add_executable(alutest alu_test.cc )
target_link_libraries(alutest PUBLIC APEX::composer)

add_executable(vector vector.cc)
target_link_libraries(vector PUBLIC APEX::composer)

