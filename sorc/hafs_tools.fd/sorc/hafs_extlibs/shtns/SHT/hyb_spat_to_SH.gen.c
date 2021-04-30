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

# This file is meta-code for SHT.c (spherical harmonic transform).
# it is intended for "make" to generate C code for 3 similar SHT functions,
# (namely spat_to_SH [Q tag]), spat_to_SHsphtor [V tag], spat_to_SH3 [both Q&V tags])
# from one generic function + tags.
# Basically, there are tags at the beginning of lines (Q,V) that are information
# to keep or remove the line depending on the function to build. (Q for scalar, V for vector, # for comment)
#
//////////////////////////////////////////////////
  #ifdef SHT_AXISYM
/// The spatial field is assumed to be \b axisymmetric (spatial size NLAT), and only the m=0 harmonics are written to output.
  #endif

/// Truncation and spatial discretization are defined by \ref shtns_create and \ref shtns_set_grid_*
/// \param[in] shtns = a configuration created by \ref shtns_create with a grid set by shtns_set_grid_*
Q/// \param[in] Vr = spatial scalar field : double array.
V/// \param[in] Vt, Vp = spatial (theta, phi) vector components : double arrays.
Q/// \param[out] Qlm = spherical harmonics coefficients :
Q/// complex double arrays of size shtns->nlm.
V/// \param[out] Slm,Tlm = spherical harmonics coefficients of \b Spheroidal and \b Toroidal scalars :
V/// complex double arrays of size shtns->nlm.
  #ifdef SHT_VAR_LTR
/// \param[in] llim = specify maximum degree of spherical harmonic. llim must be at most shtns->lmax, and all spherical harmonic degree higher than llim are set to zero. 
  #endif

QX	static void GEN3(spat_to_SH_,ID_NME,SUFFIX)(shtns_cfg shtns, double *Vr, cplx *Qlm, long int llim) {
3	static void GEN3(spat_to_SHqst_,ID_NME,SUFFIX)(shtns_cfg shtns, double *Vr, double *Vt, double *Vp, cplx *Qlm, cplx *Slm, cplx *Tlm, long int llim) {
  #ifndef SHT_GRAD
VX	static void GEN3(spat_to_SHsphtor_,ID_NME,SUFFIX)(shtns_cfg shtns, double *Vt, double *Vp, cplx *Slm, cplx *Tlm, long int llim) {
  #else
S	static void GEN3(spat_to_SHsph_,ID_NME,SUFFIX)(shtns_cfg shtns, double *Vt, cplx *Slm, long int llim) {
T	static void GEN3(spat_to_SHtor_,ID_NME,SUFFIX)(shtns_cfg shtns, double *Vp, cplx *Tlm, long int llim) {
  #endif

Q	double *zl;
V	double *dzl0;
V	struct DtDp *dzl;
	long int i,i0, ni,l;
  #ifndef SHT_AXISYM
	unsigned im, imlim;
Q	cplx *BrF;		// contains the Fourier transformed data
V	cplx *BtF, *BpF;	// contains the Fourier transformed data
Q	v2d reo[2*NLAT_2];	// symmetric (even) and anti-symmetric (odd) parts, interleaved.
V	v2d tpeo[4*NLAT_2];	// theta and phi even and odd parts
Q	#define reo0 ((double*)reo)
V	#define tpeo0 ((double*)tpeo)
V	#define te0(i)	tpeo0[4*(i)]
V	#define to0(i)	tpeo0[4*(i)+1]
V	#define pe0(i)	tpeo0[4*(i)+2]
V	#define po0(i)	tpeo0[4*(i)+3]
V	#define vteo0(i)	tpeo[2*(i)]
V	#define vpeo0(i)	tpeo[2*(i)+1]
  #else
Q	double reo0[2*NLAT_2+2] SSE;	// symmetric (even) and anti-symmetric (odd) parts, interleaved.
S	double teo0[2*NLAT_2+2] SSE;
T	double peo0[2*NLAT_2+2] SSE;
S	#define te0(i)	teo0[2*(i)]
S	#define to0(i)	teo0[2*(i)+1]
T	#define pe0(i)	peo0[2*(i)]
T	#define po0(i)	peo0[2*(i)+1]
S	#define vteo0(i)	((s2d*) teo0)[i]
T	#define vpeo0(i)	((s2d*) peo0)[i]
  #endif

QX	#define ZL(i) vdup(zl[i])
3	#define ZL(i) vdup(dzl[i].p)

// defines how to access even and odd parts of data
Q	#define re(i)	reo[2*(i)]
Q	#define ro(i)	reo[2*(i)+1]
V	#define te(i)	tpeo[4*(i)]
V	#define to(i)	tpeo[4*(i)+1]
V	#define pe(i)	tpeo[4*(i)+2]
V	#define po(i)	tpeo[4*(i)+3]
Q	#define re0(i)	reo0[2*(i)+1]
Q	#define ro0(i)	reo0[2*(i)]

  #ifndef SHT_AXISYM
Q	BrF = (cplx *) Vr;
S	BtF = (cplx *) Vt;
T	BpF = (cplx *) Vp;
	if (shtns->ncplx_fft >= 0) {
	    if (shtns->ncplx_fft > 0) {		// alloc memory for the FFT
QX	    	BrF = VMALLOC( shtns->ncplx_fft * sizeof(cplx) );
VX	    	BtF = VMALLOC( 2* shtns->ncplx_fft * sizeof(cplx) );
VX	    	BpF = BtF + shtns->ncplx_fft;
3	    	BrF = VMALLOC( 3* shtns->ncplx_fft * sizeof(cplx) );
3	    	BtF = BrF + shtns->ncplx_fft;		BpF = BtF + shtns->ncplx_fft;
	    }
Q	    fftw_execute_dft_r2c(shtns->fft,Vr, BrF);
V	    fftw_execute_dft_r2c(shtns->fft,Vt, BtF);
V	    fftw_execute_dft_r2c(shtns->fft,Vp, BpF);
	}
	imlim = MTR;
	#ifdef SHT_VAR_LTR
		if (imlim*MRES > (unsigned) llim) imlim = ((unsigned) llim)/MRES;		// 32bit mul and div should be faster
	#endif
  #endif

	ni = NLAT_2;	// copy NLAT_2 to a local variable for faster access (inner loop limit)
	//	im = 0;		// dzl.p = 0.0 : and evrything is REAL
  #ifndef SHT_NO_DCT
V	double* st_1 = shtns->st_1;
	#ifndef SHT_AXISYM
Q		#define BR0	((double *)reo)
V		#define BT0	((double *)tpeo)
V		#define BP0	((double *)tpeo + NLAT)
V		i=0;  do {	// we assume NPHI>1 (else SHT_AXISYM should be defined).
V			double sin_1 = st_1[i];
V			((double *)BtF)[i*2] *= sin_1; 	((double *)BpF)[i*2] *= sin_1;
V		} while (++i<NLAT);
Q		fftw_execute_r2r(shtns->dct_m0,(double *) BrF, BR0);		// DCT out-of-place.
V		fftw_execute_r2r(shtns->dct_m0,(double *) BtF, BT0);		// DCT out-of-place.
V		fftw_execute_r2r(shtns->dct_m0,(double *) BpF, BP0);		// DCT out-of-place.
	#else
Q		#define BR0	reo0
S		#define BT0	teo0
T		#define BP0	peo0
V		i=0;	do {
V		#ifdef _GCC_VEC_
V			s2d sin_1 = ((s2d *)st_1)[i];
S			((s2d*) Vt)[i] *= sin_1;
T		 	((s2d*) Vp)[i] *= sin_1;
V		#else
V			double sin_1 = st_1[2*i];		double sin_2 = st_1[2*i+1];
S			Vt[2*i] *= sin_1;		Vt[2*i+1] *= sin_2;
T			Vp[2*i] *= sin_1;		Vp[2*i+1] *= sin_2;
V		#endif
V		} while (++i<ni);
Q		fftw_execute_r2r(shtns->dct_m0,Vr, BR0);	// DCT out-of-place.
S		fftw_execute_r2r(shtns->dct_m0,Vt, BT0);	// DCT out-of-place.
T		fftw_execute_r2r(shtns->dct_m0,Vp, BP0);	// DCT out-of-place.
	#endif
		l=0;
		long int klim = shtns->klim;
		#ifdef SHT_VAR_LTR
			i = (llim * SHT_NL_ORDER) + 2;		// sum truncation
			if (i < klim) klim = i;
		#endif
Q		v2d* Ql = (v2d*) Qlm;
S		v2d* Sl = (v2d*) Slm;
T		v2d* Tl = (v2d*) Tlm;
Q		zl = shtns->zlm_dct0;
V		dzl0 = shtns->dzlm_dct0;
V	#ifndef _GCC_VEC_
S		cplx s1 = 0.0;		// l=0 : Sl = 0
T		cplx t1 = 0.0;		// l=0 : Tl = 0
V	#else
S		v2d s = vdup(0.0);		// l=0 : Sl = 0
T		v2d t = vdup(0.0);		// l=0 : Tl = 0
V	#endif
QX		BR0[klim] = 0;		BR0[klim+1] = 0;		// allow some overflow.
	#ifdef SHT_VAR_LTR
		while(l < llim) {
	#else
		do {		// l < LMAX
	#endif
			i=l;	// l < klim
	  #ifndef _GCC_VEC_
S			Sl[l] = s1;
T			Tl[l] = t1;
Q			cplx q0 = 0.0;	cplx q1 = 0.0;
S			cplx s0 = 0.0;	s1 = 0.0;
T			cplx t0 = 0.0;	t1 = 0.0;
			do {
Q				q0 += BR0[i]   * zl[i];
Q				q1 += BR0[i+1] * zl[i+1];
S				s0 += BT0[i]   * dzl0[i];
T				t0 -= BP0[i]   * dzl0[i];
S				s1 += BT0[i+1] * dzl0[i+1];
T				t1 -= BP0[i+1] * dzl0[i+1];
				i+=2;
			} while(i<klim);
Q			Ql[l] = q0;		Ql[l+1] = q1;
S			Sl[l+1] = s0;
T			Tl[l+1] = t0;
	  #else
S			Sl[l] = vhi_to_cplx(s);
T			Tl[l] = vhi_to_cplx(t);
S			s = vdup(0.0);
T			t = vdup(0.0);
Q			s2d q = vdup(0.0);
QX			s2d q1 = vdup(0.0);
			i >>= 1;	// i = i/2
			do {
Q				q += ((s2d*) zl)[i] * ((s2d*) BR0)[i];
S				s += ((s2d*) dzl0)[i] * ((s2d*) BT0)[i];
T				t -= ((s2d*) dzl0)[i] * ((s2d*) BP0)[i];
				++i;
QX				q1 += ((s2d*) zl)[i] * ((s2d*) BR0)[i];
QX				++i;
			} while(2*i < klim);
QX			q += q1;
Q			Ql[l]   = vlo_to_cplx(q);		Ql[l+1] = vhi_to_cplx(q);
S			Sl[l+1] = vlo_to_cplx(s);
T			Tl[l+1] = vlo_to_cplx(t);
	  #endif
			l+=2;
Q			zl += (shtns->klim - l);
V			dzl0 += (shtns->klim -l);
	#ifndef SHT_VAR_LTR
		} while(l<llim);
	#else
		}
	#endif
		if (l == llim) {
V	#ifndef _GCC_VEC_
S			Sl[l] = s1;
T			Tl[l] = t1;
V	#else
S			((v2d*) Sl)[l] = vhi_to_cplx(s);
T			((v2d*) Tl)[l] = vhi_to_cplx(t);
V	#endif
Q			cplx q0 = 0.0;
Q			i=l;	// l < klim
Q			do {
Q				q0 += BR0[i] * zl[i];
Q				i+=2;
Q			} while(i<klim);
Q			((cplx *) Ql)[l] = q0;
			++l;
		}
Q	#undef BR0
S	#undef BT0
T	#undef BP0
	#ifdef SHT_VAR_LTR
		while( l<=LMAX ) {
Q			Ql[l] = vdup(0.0);
S			Sl[l] = vdup(0.0);
T			Tl[l] = vdup(0.0);
			++l;
		}
	#endif
  #else		// ifndef SHT_NO_DCT
		i=0;
Q		zl = shtns->zlm[0];
		// stride of source data : we assume NPHI>1 (else SHT_AXISYM should be defined).
	#ifndef SHT_AXISYM
Q		#define BR0(i) ((double*)BrF)[(i)*2]
S		#define BT0(i) ((double*)BtF)[(i)*2]
T		#define BP0(i) ((double*)BpF)[(i)*2]
	#else
Q		#define BR0(i) Vr[i]
S		#define BT0(i) Vt[i]
T		#define BP0(i) Vp[i]
	#endif
	#if _GCC_VEC_ && __SSE3__
Q		s2d r0v = vdup(0.0);
		do {	// compute symmetric and antisymmetric parts.
Q			s2d a = vdup(BR0(i));		s2d b = vdup(BR0(NLAT-1-i));
QX			s2d g = vdup(BR0(i+1));		s2d h = vdup(BR0(NLAT-2-i));
Q			a = subadd(a,b);
QX			g = subadd(g,h);
Q			((s2d*) reo0)[i] = a;		// assume odd is first, then even.
3			r0v += vdup(zl[i]) * a;	// even part is used.
QX			((s2d*) reo0)[i+1] = g;		// assume odd is first, then even.
QX			a = _mm_unpackhi_pd(a, g);
QX			r0v += *((s2d*)(zl+i)) * a;	// even part is used, reduce data dependency
S			s2d c = vdup(BT0(i));		s2d d = vdup(BT0(NLAT-1-i));
S			c = subadd(c,d);		vteo0(i) = vxchg(c);
T			s2d e = vdup(BP0(i));		s2d f = vdup(BP0(NLAT-1-i));
T			e = subadd(e,f);		vpeo0(i) = vxchg(e);
V			++i;
QX			i+=2;
		} while(i<ni);
QX		r0v += vxchg(r0v);
QX		((s2d*) reo0)[ni] = vdup(0.0);		// allow some overflow.
Q		((v2d*)Qlm)[0] = vhi_to_cplx(r0v);
	#else
Q		double r0 = 0.0;
QX		double r1 = 0.0;
		do {	// compute symmetric and antisymmetric parts.
Q			double a = BR0(i);		double b = BR0(NLAT-1-i);
Q			ro0(i) = (a-b);		re0(i) = (a+b);
Q			r0 += zl[i] * (a+b);
S			double c = BT0(i);		double d = BT0(NLAT-1-i);
S			te0(i) = (c+d);		to0(i) = (c-d);
T			double e = BP0(i);		double f = BP0(NLAT-1-i);
T			pe0(i) = (e+f);		po0(i) = (e-f);
		} while(++i<ni);
QX		r0 += r1;
QX		ro0(ni) = 0.0;		re0(ni) = 0.0;		// allow some overflow.
Q		Qlm[0] = r0;
	#endif
Q		#undef BR0
S		#undef BT0
T		#undef BP0
Q		zl += ni + (ni&1);		// SSE alignement
		l=1;			// l=0 is zero for the vector transform.
Q		v2d* Ql = (v2d*) Qlm;		// virtual pointer for l=0 and im
S		v2d* Sl = (v2d*) Slm;		// virtual pointer for l=0 and im
T		v2d* Tl = (v2d*) Tlm;		// virtual pointer for l=0 and im
V		dzl0 = (double *) shtns->dzlm[0];		// only theta derivative (d/dphi = 0 for m=0)
S		Sl[0] = vdup(0.0);	// l=0 is zero for the vector transform.
T		Tl[0] = vdup(0.0);	// l=0 is zero for the vector transform.
	#ifdef SHT_VAR_LTR
		while (l<llim) {		// ops : NLAT/2 * (2*(LMAX-m+1) + 4) : almost twice as fast.
	#else
		do {
	#endif
			i=0;
  #ifndef _GCC_VEC_
Q			double q0 = 0.0;
Q			double q1 = 0.0;
S			double s0 = 0.0;	double s1 = 0.0;
T			double t0 = 0.0;	double t1 = 0.0;
			do {
Q				q0 += zl[0] * ro0(i);	// Qlm[LiM(l,im)] += zlm[im][(l-m)*NLAT/2 + i] * fp[i];
Q				q1 += zl[1] * re0(i);	// Qlm[LiM(l+1,im)] += zlm[im][(l+1-m)*NLAT/2 + i] * fm[i];
S				s0 += dzl0[0] * te0(i);
T				t0 -= dzl0[0] * pe0(i);
S				s1 += dzl0[1] * to0(i);
T				t1 -= dzl0[1] * po0(i);
Q				zl +=2;
V				dzl0 +=2;
			} while(++i < ni);
Q			Ql[l] = q0;
Q			Ql[l+1] = q1;
S			Sl[l] = s0;		Sl[l+1] = s1;
T			Tl[l] = t0;		Tl[l+1] = t1;
  #else
S			s2d s = vdup(0.0);
T			s2d t = vdup(0.0);
Q			s2d q = vdup(0.0);
QX			s2d q1 = vdup(0.0);
			do {
S				s += ((s2d*) dzl0)[i] * vteo0(i);
T				t -= ((s2d*) dzl0)[i] * vpeo0(i);
Q				q += ((s2d*) zl)[i] * ((s2d*) reo0)[i];
				++i;
QX				q1 += ((s2d*) zl)[i] * ((s2d*) reo0)[i];		// reduce dependency
QX				++i;
			} while(i < ni);
QX			q += q1;
Q			zl += 2*ni;
V			dzl0 += 2*ni;
Q			Ql[l] = vlo_to_cplx(q);		Ql[l+1] = vhi_to_cplx(q);
S			Sl[l] = vlo_to_cplx(s);		Sl[l+1] = vhi_to_cplx(s);
T			Tl[l] = vlo_to_cplx(t);		Tl[l+1] = vhi_to_cplx(t);
  #endif
			l+=2;
	#ifndef SHT_VAR_LTR
		} while (l<llim);
	#else
		}
	#endif
		if (l==llim) {
			long int lstride=1;
	  #ifdef SHT_VAR_LTR
			if (l != LMAX) lstride=2;
	  #endif
QX			double q1 = 0.0;		// reduce dependency by unrolling loop
Q			double q0 = 0.0;
S			double s0 = 0.0;
T			double t0 = 0.0;
			i=0;	do {
Q				q0 += zl[0] * ro0(i);		// Qlm[LiM(l,im)] += zlm[im][(l-m)*NLAT/2 + i] * fp[i];
S				s0 += dzl0[0] * te0(i);
T				t0 -= dzl0[0] * pe0(i);
Q				zl += lstride;
V				dzl0 += lstride;
				++i;
QX				q1 += zl[0] * ro0(i);
QX				zl += lstride;
QX				++i;
			} while(i<ni);
QX			q0 += q1;
Q			((cplx *)Ql)[l] = q0;
S			((cplx *)Sl)[l] = s0;
T			((cplx *)Tl)[l] = t0;
	  #ifdef SHT_VAR_LTR
	  		++l;
		}
	    while( l<=LMAX ) {
Q			Ql[l] = vdup(0.0);
S			Sl[l] = vdup(0.0);
T			Tl[l] = vdup(0.0);
			++l;
      #endif
		}
  #endif		// ifndef SHT_NO_DCT
  #ifndef SHT_AXISYM
	for (im=1;im<=imlim;++im) {
Q		BrF += NLAT;
V		BtF += NLAT;	BpF += NLAT;
		i0 = shtns->tm[im];
 		i=i0;
		do {	// compute symmetric and antisymmetric parts.
3			s2d sin = vdup(shtns->st[i]);
3			v2d q0 = ((v2d *)BrF)[i];	v2d q1 = ((v2d *)BrF)[NLAT-1-i];		re(i) = (q0+q1)*sin;	ro(i) = (q0-q1)*sin;
QX			v2d q0 = ((v2d *)BrF)[i];	v2d q1 = ((v2d *)BrF)[NLAT-1-i];		re(i) = q0+q1;	ro(i) = q0-q1;		  
V			v2d t0 = ((v2d *)BtF)[i];	v2d t1 = ((v2d *)BtF)[NLAT-1-i];		te(i) = t0+t1;	to(i) = t0-t1;
V			v2d s0 = ((v2d *)BpF)[i];	v2d s1 = ((v2d *)BpF)[NLAT-1-i];		pe(i) = s0+s1;	po(i) = s0-s1;
 		} while (++i<ni);
		l = LiM(shtns, 0,im);
Q		v2d* Ql = (v2d*) &Qlm[l];	// virtual pointer for l=0 and im
V		v2d* Sl = (v2d*) &Slm[l];		v2d* Tl = (v2d*) &Tlm[l];
		l=im*MRES;
3		double m_1 = 1.0/l;
Q		zl = shtns->zlm[im];
V		dzl = shtns->dzlm[im];
		while (l<llim) {		// ops : NLAT/2 * (2*(LMAX-m+1) + 4) : almost twice as fast.
Q			v2d q0 = vdup(0.0);
Q			v2d q1 = vdup(0.0);
V			v2d s0 = vdup(0.0);	v2d t1 = vdup(0.0);		v2d s0i = vdup(0.0);	v2d t1i = vdup(0.0);
V			v2d t0 = vdup(0.0);	v2d s1 = vdup(0.0);		v2d t0i = vdup(0.0);	v2d s1i = vdup(0.0);
			i=i0;	do {		// tm[im] : polar optimization
V				s0  += vdup(dzl[0].t) *to(i);		// ref: these E. Dormy p 72.
V				t0  -= vdup(dzl[0].t) *po(i);
V				s1  += vdup(dzl[1].t) *te(i);
V				t1  -= vdup(dzl[1].t) *pe(i);
V				s0i -= vdup(dzl[0].p) *pe(i);
V				t0i -= vdup(dzl[0].p) *te(i);
V				s1i -= vdup(dzl[1].p) *po(i);
V				t1i -= vdup(dzl[1].p) *to(i);
Q				q0  += re(i) * ZL(0);		// Qlm[LiM(l,im)] += zlm[im][(l-m)*NLAT/2 + i] * fp[i];
Q				q1  += ro(i) * ZL(1);		// Qlm[LiM(l+1,im)] += zlm[im][(l+1-m)*NLAT/2 + i] * fm[i];
Q				zl +=2;
V				dzl +=2;
			} while (++i < ni);
3			q0 *= vdup((l*(l+1))*m_1);
3			q1 *= vdup(((l+1)*(l+2))*m_1);
V			Sl[l] = addi(s0,s0i);	Tl[l+1] = addi(t1,t1i);
V			Tl[l] = addi(t0,t0i);	Sl[l+1] = addi(s1,s1i);
Q			Ql[l] = q0;
Q			Ql[l+1] = q1;
			l+=2;
		}
		if (l==llim) {
			long int lstride=1;
	  #ifdef SHT_VAR_LTR
			if (l != LMAX) lstride=2;
	  #endif
Q			v2d q0 = vdup(0.0);	// Qlm[LiM(l,im)] = 0.0;
V			v2d s0 = vdup(0.0);	v2d s0i = vdup(0.0);
V			v2d t0 = vdup(0.0);	v2d t0i = vdup(0.0);
			i=i0;	do {		// tm[im] : polar optimization
V				s0  += vdup(dzl[0].t) *to(i);
V				t0  -= vdup(dzl[0].t) *po(i);
V				s0i -= vdup(dzl[0].p) *pe(i);
V				t0i -= vdup(dzl[0].p) *te(i);
Q				q0  += re(i) * ZL(0);		// Qlm[LiM(l,im)] += zlm[im][(l-m)*NLAT/2 + i] * fp[i];
Q				zl  += lstride;
V				dzl += lstride;
			} while(++i<ni);
3			q0 *= vdup((l*(l+1))*m_1);
V			Sl[l] = addi(s0,s0i);
V			Tl[l] = addi(t0,t0i);
Q			Ql[l] = q0;
	  #ifdef SHT_VAR_LTR
	  		++l;
		}
	    while( l<=LMAX ) {
Q			Ql[l] = vdup(0.0);
V			Sl[l] = vdup(0.0);	Tl[l] = vdup(0.0);
			++l;
      #endif
		}
	}
	#ifdef SHT_VAR_LTR
	if (imlim < MMAX) {
		im = imlim+1;
		l = LiM(shtns, im*MRES, im);
		do {
Q			((v2d*)Qlm)[l] = vdup(0.0);
V			((v2d*)Slm)[l] = vdup(0.0);		((v2d*)Tlm)[l] = vdup(0.0);
		} while(++l < shtns->nlm);
	}
	#endif

  	if (shtns->ncplx_fft > 0) {		// free memory
Q	    VFREE(BrF - NLAT*imlim);
VX	    VFREE(BtF - NLAT*imlim);	// this frees also BpF.
	}
  #endif

Q	#undef ZL
Q	#undef re
Q	#undef ro
V	#undef te
V	#undef to
V	#undef pe
V	#undef po
Q	#undef re0
Q	#undef ro0
V	#undef te0
V	#undef to0
V	#undef pe0
V	#undef po0
Q	#undef reo0
V	#undef teo0
V	#undef peo0
Q	#undef reo0
V	#undef tpeo0
V	#undef vteo0
V	#undef vpeo0
  }
