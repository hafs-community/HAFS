option(tests          "Enable testing."                         on)

if(tests) 
  include(${cookoff_path}/AddGTest.cmake)
  enable_testing()
endif(tests)

file(WRITE ${CMAKE_SOURCE_DIR}/mytest.cc 
     "#include <gtest/gtest.h>\n\n"
     "class TestMe : public ::testing::Test {};\n\n"
     "TEST_F(TestMe, TestThis) {\n"
     "  EXPECT_TRUE(true); \n"
     "}\n\n"
     "int main(int argc, char **argv) {\n"
     "  ::testing::InitGoogleTest(&argc, argv);\n"
     "  return RUN_ALL_TESTS();\n"
     "}\n")


add_gtest(mytest mytest.cc)
