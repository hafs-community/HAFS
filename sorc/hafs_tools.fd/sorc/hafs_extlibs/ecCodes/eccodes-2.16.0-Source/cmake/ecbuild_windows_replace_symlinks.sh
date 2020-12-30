#
# Bash script to replace all symlinks with copies on Windows.
#
# Cmake on non-cygwin windows will completely ignore symlinks, causing install to fail for any repos
# with symlinks in the source (e.g. ecCodes). See SystemTools::CreateSymlink [0], which at time of
# writing is:
#
# ```
# #if defined(_WIN32) && !defined(__CYGWIN__)
# bool SystemTools::CreateSymlink(const std::string&, const std::string&)
# {
#   return false;
# }
# ```
#
# [0] https://github.com/Kitware/CMake/blob/master/Source/kwsys/SystemTools.cxx#L2969
#
echo ""
echo "======================="
echo " REMOVING ALL SYMLINKS "
echo "======================="
echo ""

for link in `find . -type l -not -path './build/*'`
do
    if [ -e $link ]
    then
        echo "removing link $link"
        target=$(readlink -f $link)
        # Remove the symlink before copying (rather than passing --remove-destination to cp)
        # to prevent cp complaining about overwriting a symlink with a directory.
        rm -r $link
        cp -rTL $target $link
    else
        echo "ignoring broken link $link"
    fi
done
