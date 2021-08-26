cmake_minimum_required(VERSION 3.19)
project(llvm_first_lang)

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_FLAGS "-fpermissive -O0 -DDEBUG")

#set (CMAKE_PREFIX_PATH "/usr/local")
set (CMAKE_PREFIX_PATH "/Users/uh/LLVM/llvm-project-12.0.1/llvm/cmake-build-llvm-build")
find_package(LLVM REQUIRED CONFIG)
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

add_executable(llvm_first_lang my-lang.cpp)
#add_executable(llvm_first_lang toy.cpp)

llvm_map_components_to_libnames(llvm_libs
        support
        core
        irreader
        native
        Analysis
        Core
        ExecutionEngine
        InstCombine
        Object
        OrcJIT
        RuntimeDyld
        ScalarOpts
        Support
        TransformUtils)

target_link_libraries(llvm_first_lang ${llvm_libs})
#set(CMAKE_PREFIX_PATH "/usr/local")
#set(FLEX_EXECUTABLE "/usr/local/Cellar/flex/2.6.4_2/bin/flex")
#find_package(FLEX REQUIRED)
#flex_target(quadscanner quadscanner.ll ${CMAKE_CURRENT_BINARY_DIR}/quadscanner.cpp)


#set(FLEX_INCLUDE_DIRS /usr/local/Cellar/flex/2.6.4_2/include)
#add_executable(quadscanner ${FLEX_quadscanner_OUTPUTS} quadgen.cpp quadgen.h)
#set(FLEX_LIBRARIES  /usr/local/Cellar/flex/2.6.4_2/lib/libfl.dylib)
#target_link_libraries(quadscanner ${FLEX_LIBRARIES} ${llvm_libs})