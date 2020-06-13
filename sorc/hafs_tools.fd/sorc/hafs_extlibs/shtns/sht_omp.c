/*
 * Copyright (c) 2010-2018 Centre National de la Recherche Scientifique.
 * written by Nathanael Schaeffer (CNRS, ISTerre, Grenoble, France).
 * 
 * nathanael.schaeffer@univ-grenoble-alpes.fr
 * 
 * This software is governed by the CeCILL license under French law and
 * abiding by the rules of distribution of free software. You can use,
 * modify and/or redistribute the software under the terms of the CeCILL
 * license as circulated by CEA, CNRS and INRIA at the following URL
 * "http://www.cecill.info".
 * 
 * The fact that you are presently reading this means that you have had
 * knowledge of the CeCILL license and that you accept its terms.
 * 
 */

#include "sht_private.h"

#define MTR MMAX
#define SHT_VAR_LTR

#define GEN(name,sfx) GLUE2(name,sfx)
#define GEN3(name,nw,sfx) GLUE3(name,nw,sfx)

// genaral case
#undef SUFFIX
#define SUFFIX _l

	#define NWAY 1
	#include "SHT/spat_to_SHst_omp.c"
	#ifdef _GCC_VEC_
	#include "SHT/SHst_to_spat_omp.c"
	#endif
	#undef NWAY
	#define NWAY 2
	#include "SHT/spat_to_SH_omp.c"
	#include "SHT/SH_to_spat_omp.c"
	#include "SHT/spat_to_SHst_omp.c"
	#include "SHT/SHst_to_spat_omp.c"
	#undef NWAY
	#define NWAY 3
	#include "SHT/spat_to_SH_omp.c"
	#include "SHT/spat_to_SHst_omp.c"
	#ifdef _GCC_VEC_
	#include "SHT/SH_to_spat_omp.c"
	#include "SHT/SHst_to_spat_omp.c"
	#endif
	#undef NWAY
	#define NWAY 4
	#include "SHT/spat_to_SH_omp.c"
	#include "SHT/SH_to_spat_omp.c"
	#include "SHT/spat_to_SHst_omp.c"
	#include "SHT/SHst_to_spat_omp.c"
	#undef NWAY
	#define NWAY 6
	#include "SHT/spat_to_SH_omp.c"
	#include "SHT/SH_to_spat_omp.c"
	#include "SHT/spat_to_SHst_omp.c"
	#include "SHT/SHst_to_spat_omp.c"
	#undef NWAY
	#define NWAY 8
	#include "SHT/spat_to_SH_omp.c"
	#include "SHT/SH_to_spat_omp.c"
	#include "SHT/spat_to_SHst_omp.c"
	#include "SHT/SHst_to_spat_omp.c"
	#undef NWAY

#define SHT_GRAD
	#define NWAY 2
	#include "SHT/SHs_to_spat_omp.c"
	#include "SHT/SHt_to_spat_omp.c"
	#undef NWAY
	#ifdef _GCC_VEC_
	#define NWAY 1
	#include "SHT/SHs_to_spat_omp.c"
	#include "SHT/SHt_to_spat_omp.c"
	#undef NWAY
	#define NWAY 3
	#include "SHT/SHs_to_spat_omp.c"
	#include "SHT/SHt_to_spat_omp.c"
	#undef NWAY
	#endif
	#define NWAY 4
	#include "SHT/SHs_to_spat_omp.c"
	#include "SHT/SHt_to_spat_omp.c"
	#undef NWAY
#undef SHT_GRAD

#define SHT_3COMP
	#define NWAY 1
	#include "SHT/spat_to_SHqst_omp.c"
	#ifdef _GCC_VEC_
	#include "SHT/SHqst_to_spat_omp.c"
	#endif
	#undef NWAY
	#define NWAY 2
	#include "SHT/spat_to_SHqst_omp.c"
	#include "SHT/SHqst_to_spat_omp.c"
	#undef NWAY
	#define NWAY 3
	#include "SHT/spat_to_SHqst_omp.c"
	#ifdef _GCC_VEC_
	#include "SHT/SHqst_to_spat_omp.c"
	#endif
	#undef NWAY
	#define NWAY 4
	#include "SHT/spat_to_SHqst_omp.c"
	#include "SHT/SHqst_to_spat_omp.c"
	#undef NWAY
	#define NWAY 6
	#include "SHT/spat_to_SHqst_omp.c"
	#include "SHT/SHqst_to_spat_omp.c"
	#undef NWAY
	#define NWAY 8
	#include "SHT/spat_to_SHqst_omp.c"
	#include "SHT/SHqst_to_spat_omp.c"
	#undef NWAY
#undef SHT_3COMP

#ifndef _GCC_VEC_
	#define SHsphtor_to_spat_omp1_l NULL
	#define SHsph_to_spat_omp1_l NULL
	#define SHtor_to_spat_omp1_l NULL
	#define SHqst_to_spat_omp1_l NULL
	#define SH_to_spat_omp3_l NULL
	#define SHsphtor_to_spat_omp3_l NULL
	#define SHsph_to_spat_omp3_l NULL
	#define SHtor_to_spat_omp3_l NULL
	#define SHqst_to_spat_omp3_l NULL
#endif

void* fomp[6][SHT_NTYP] = {
	{ NULL, NULL, SHsphtor_to_spat_omp1_l, spat_to_SHsphtor_omp1_l,
		SHsph_to_spat_omp1_l, SHtor_to_spat_omp1_l, SHqst_to_spat_omp1_l, spat_to_SHqst_omp1_l },
	{ SH_to_spat_omp2_l, spat_to_SH_omp2_l, SHsphtor_to_spat_omp2_l, spat_to_SHsphtor_omp2_l,
		SHsph_to_spat_omp2_l, SHtor_to_spat_omp2_l, SHqst_to_spat_omp2_l, spat_to_SHqst_omp2_l },
	{ SH_to_spat_omp3_l, spat_to_SH_omp3_l, SHsphtor_to_spat_omp3_l, spat_to_SHsphtor_omp3_l,
		SHsph_to_spat_omp3_l, SHtor_to_spat_omp3_l, SHqst_to_spat_omp3_l, spat_to_SHqst_omp3_l },
	{ SH_to_spat_omp4_l, spat_to_SH_omp4_l, SHsphtor_to_spat_omp4_l, spat_to_SHsphtor_omp4_l,
		SHsph_to_spat_omp4_l, SHtor_to_spat_omp4_l, SHqst_to_spat_omp4_l, spat_to_SHqst_omp4_l },
	{ SH_to_spat_omp6_l, spat_to_SH_omp6_l, SHsphtor_to_spat_omp6_l, spat_to_SHsphtor_omp6_l,
		NULL, NULL, SHqst_to_spat_omp6_l, spat_to_SHqst_omp6_l },
	{ SH_to_spat_omp8_l, spat_to_SH_omp8_l, SHsphtor_to_spat_omp8_l, spat_to_SHsphtor_omp8_l,
		NULL, NULL, SHqst_to_spat_omp8_l, spat_to_SHqst_omp8_l }
};
