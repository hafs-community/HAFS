=============================
ecbuild 2 compatibility layer
=============================

The files in this directory here emulate the behaviour of ecbuild 2.x in order
to ensure smooth transition to ecbuild 3. As such, they are deprecated by
definition.

The compatibility layer is enabled by default. To disable it, set the
``ECBUILD_2_COMPAT`` option to ``OFF``. To output deprecation warnings instead,
set the ``ECBUILD_2_COMPAT_DEPRECATE`` option to ``ON``.
