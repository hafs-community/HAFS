cmake_minimum_required(VERSION 3.6 FATAL_ERROR)

#[=[
From https://sourceware.org/bugzilla/show_bug.cgi?id=20535

> $ cat dso1.c
> void fun1(void) {}
>
> $ cat dso2.c
> void fun1(void);
> void fun2(void) { fun1(); }
>
> $ cat main.c
> void fun2(void);
> int main(void) {
>  fun2();
>  return 0;
> }
>
> $ mkdir dsos
> $ gcc -shared -fPIC -o dsos/libdso1.so dso1.c
> $ gcc -shared -fPIC -o dsos/libdso2.so -Wl,-rpath='$ORIGIN' dso2.c -Ldsos -ldso1
> $ gcc -Wl,-rpath='$ORIGIN/dsos' main.c -Ldsos -ldso2
#]=]

set(CMAKE_INSTALL_RPATH_USE_LINK_PATH FALSE)
set(CMAKE_SKIP_BUILD_RPATH FALSE)
set(CMAKE_BUILD_WITH_INSTALL_RPATH TRUE)

project(test_ld_origin VERSION 1.0 LANGUAGES C)

add_library(dso1 SHARED dso1.c)

add_library(dso2 SHARED dso2.c)
target_link_libraries(dso2 PRIVATE dso1)
set_target_properties(dso2 PROPERTIES
    INSTALL_RPATH "$ORIGIN")

add_executable(main main.c)
target_link_libraries(main dso2)
set_target_properties(main PROPERTIES
    INSTALL_RPATH "$ORIGIN/../lib")

set_target_properties(dso1 dso2 main PROPERTIES
    LIBRARY_OUTPUT_DIRECTORY lib
    RUNTIME_OUTPUT_DIRECTORY bin)

install(TARGETS dso1 dso2 main
    LIBRARY DESTINATION lib
    RUNTIME DESTINATION bin)
