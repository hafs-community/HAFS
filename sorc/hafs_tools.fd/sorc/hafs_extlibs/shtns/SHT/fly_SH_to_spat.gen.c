/*
 * Copyright (c) 2010-2019 Centre National de la Recherche Scientifique.
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
# it is intended for "make" to generate C code for similar SHT functions,
# from one generic function + tags.
# > See Makefile and SHT.c
# Basically, there are tags at the beginning of lines that are information
# to keep or remove the line depending on the function to build.
# tags :
# Q : line for scalar transform
# V : line for vector transform (both spheroidal and toroidal)
# S : line for vector transfrom, spheroidal component
# T : line for vector transform, toroidal component.

3	static void GEN3(SHqst_to_spat_fly,NWAY,SUFFIX)(shtns_cfg shtns, cplx *Qlm, cplx *Slm, cplx *Tlm, double *Vr, double *Vt, double *Vp, const long int llim) {
QX	static void GEN3(SH_to_spat_fly,NWAY,SUFFIX)(shtns_cfg shtns, cplx *Qlm, double *Vr, const long int llim) {
  #ifndef SHT_GRAD
VX	static void GEN3(SHsphtor_to_spat_fly,NWAY,SUFFIX)(shtns_cfg shtns, cplx *Slm, cplx *Tlm, double *Vt, double *Vp, const long int llim) {
  #else
S	static void GEN3(SHsph_to_spat_fly,NWAY,SUFFIX)(shtns_cfg shtns, cplx *Slm, double *Vt, double *Vp, const long int llim) {
T	static void GEN3(SHtor_to_spat_fly,NWAY,SUFFIX)(shtns_cfg shtns, cplx *Tlm, double *Vt, double *Vp, const long int llim) {
  #endif

	#if !defined( _GCC_VEC_ ) && (NWAY & 1)
	#error "NWAY must be even when compiled without explicit vectorization."
	#endif

  #ifndef SHT_AXISYM
Q	v2d *BrF;
V	v2d *BtF, *BpF;
  #ifndef SHTNS_ISHIOKA
Q	#define qr(l) vall(creal(Ql[l]))
Q	#define qi(l) vall(cimag(Ql[l]))
  #else
Q	#define qr(l) vall( ((double*) QQl)[2*(l)]   )
Q	#define qi(l) vall( ((double*) QQl)[2*(l)+1] )
  #endif
V	#define vr(l) vall( ((double*) VWl)[4*(l)]   )
V	#define vi(l) vall( ((double*) VWl)[4*(l)+1] )
V	#define wr(l) vall( ((double*) VWl)[4*(l)+2] )
V	#define wi(l) vall( ((double*) VWl)[4*(l)+3] )
	unsigned im, imlim;
  #else
Q	double* const BrF = Vr;
S	double* const BtF = Vt;
T	double* const BpF = Vp;
  #endif
	long int nk, k,l,m;
	double *alm, *al;
	s2d *ct, *st;
V	int robert_form;
QX	double Ql0[llim+2];
V	v2d VWl[llim*2+4];
  #ifdef SHTNS_ISHIOKA
Q	v2d QQl[llim+2];
  #endif

  #ifndef SHT_AXISYM
Q	BrF = (v2d*) Vr;
V	BtF = (v2d*) Vt;	BpF = (v2d*) Vp;
	if (shtns->fftc_mode > 0) {		// alloc memory for the FFT
		unsigned long nv = shtns->nspat;
QX		BrF = (v2d*) VMALLOC( nv * sizeof(double) );
VX		BtF = (v2d*) VMALLOC( 2*nv * sizeof(double) );
VX		BpF = BtF + nv/2;
3		BrF = (v2d*) VMALLOC( 3*nv * sizeof(double) );
3		BtF = BrF + nv/2;		BpF = BrF + nv;
	}
	  #ifdef SHT_GRAD
S		k=0; do { BpF[k]=vdup(0.0); } while(++k<NLAT_2);
T		k=0; do { BtF[k]=vdup(0.0); } while(++k<NLAT_2);
	  #endif
	imlim = MTR;
	#ifdef SHT_VAR_LTR
		if (imlim*MRES > (unsigned) llim) imlim = ((unsigned) llim)/MRES;		// 32bit mul and div should be faster
	#endif
  #else
	#ifdef SHT_GRAD
S		if (Vp != NULL) { k=0; do { ((v2d*)Vp)[k]=vdup(0.0); } while(++k<NLAT_2); }
T		if (Vt != NULL) { k=0; do { ((v2d*)Vt)[k]=vdup(0.0); } while(++k<NLAT_2); }
	#endif
  #endif
V	robert_form = shtns->robert_form;

	ct = (s2d*) shtns->ct;		st = (s2d*) shtns->st;
	{	//	im=0;
		alm = shtns->alm;
S		double* const Sl0 = (double*) VWl;
T		double* const Tl0 = (double*) VWl + llim+2;
3		double* const Ql0 = (double*) (VWl + llim+2);
 		l=1;
Q		Ql0[0] = (double) Qlm[0];		// l=0
		do {		// for m=0, compress the complex Q,S,T to double
Q			Ql0[l] = (double) Qlm[l];	//	Ql[l+1] = (double) Qlm[l+1];
S			Sl0[l-1] = (double) Slm[l];	//	Sl[l] = (double) Slm[l+1];
T			Tl0[l-1] = (double) Tlm[l];	//	Tl[l] = (double) Tlm[l+1];
			++l;
		} while(l<=llim);
		k=0;	nk = NLAT_2;
		nk = ((unsigned)(nk+VSIZE2-1)) / VSIZE2;
		do {
			l=0;	al = alm;
			rnd cost[NWAY], y0[NWAY], y1[NWAY];
V			rnd sint[NWAY], dy0[NWAY], dy1[NWAY];
Q			rnd re[NWAY], ro[NWAY];
S			rnd te[NWAY], to[NWAY];
T			rnd pe[NWAY], po[NWAY];
			for (int j=0; j<NWAY; ++j) {
				cost[j] = vread(ct, j+k);
V				sint[j] = -vread(st, j+k);
				y0[j] = vall(al[0]);
V				dy0[j] = vall(0.0);
Q				re[j] = y0[j] * vall(Ql0[0]);
S				to[j] = dy0[j];
T				po[j] = dy0[j];
			}
V			if (robert_form) {
V				for (int j=0; j<NWAY; ++j) sint[j] *= -sint[j];
V			}
			for (int j=0; j<NWAY; ++j) {
				y1[j]  = vall(al[0]*al[1]) * cost[j];
V				dy1[j] = vall(al[0]*al[1]) * sint[j];
			}
			for (int j=0; j<NWAY; ++j) {
Q				ro[j] = y1[j] * vall(Ql0[1]);
S				te[j] = dy1[j] * vall(Sl0[0]);
T				pe[j] = -dy1[j] * vall(Tl0[0]);
			}
			al+=2;	l+=2;
			while(l<llim) {
				for (int j=0; j<NWAY; ++j) {
V					dy0[j] = vall(al[1])*(cost[j]*dy1[j] + y1[j]*sint[j]) + vall(al[0])*dy0[j];
					y0[j]  = vall(al[1])*(cost[j]*y1[j]) + vall(al[0])*y0[j];
				}
				for (int j=0; j<NWAY; ++j) {
Q					re[j] += y0[j] * vall(Ql0[l]);
S					to[j] += dy0[j] * vall(Sl0[l-1]);
T					po[j] -= dy0[j] * vall(Tl0[l-1]);
				}
				for (int j=0; j<NWAY; ++j) {
V					dy1[j] = vall(al[3])*(cost[j]*dy0[j] + y0[j]*sint[j]) + vall(al[2])*dy1[j];
					y1[j]  = vall(al[3])*(cost[j]*y0[j]) + vall(al[2])*y1[j];
				}
				for (int j=0; j<NWAY; ++j) {
Q					ro[j] += y1[j] * vall(Ql0[l+1]);
S					te[j] += dy1[j] * vall(Sl0[l]);
T					pe[j] -= dy1[j] * vall(Tl0[l]);
				}
				al+=4;	l+=2;
			}
			if (l==llim) {
				for (int j=0; j<NWAY; ++j) {
V					dy0[j] = vall(al[1])*(cost[j]*dy1[j] + y1[j]*sint[j]) + vall(al[0])*dy0[j];
					y0[j]  = vall(al[1])*cost[j]*y1[j] + vall(al[0])*y0[j];
				}
				for (int j=0; j<NWAY; ++j) {
Q					re[j] += y0[j] * vall(Ql0[l]);
S					to[j] += dy0[j] * vall(Sl0[l-1]);
T					po[j] -= dy0[j] * vall(Tl0[l-1]);
				}
			}
		#ifndef SHTNS4MAGIC
			for (int j=0; j<NWAY; ++j) {
Q				S2D_STORE(BrF, j+k, re[j], ro[j])
S				S2D_STORE(BtF, j+k, te[j], to[j])
T				S2D_STORE(BpF, j+k, pe[j], po[j])
			}
		#else
			for (int j=0; j<NWAY; ++j) {
				if ((k+j)>=nk) break;
Q				S2D_STORE_4MAGIC((double*)BrF, j+k, re[j], ro[j]);
S				S2D_STORE_4MAGIC((double*)BtF, j+k, te[j], to[j]);
T				S2D_STORE_4MAGIC((double*)BpF, j+k, pe[j], po[j]);
			}
		#endif
			k+=NWAY;
		} while (k < nk);
	}

  #ifndef SHT_AXISYM
Q		BrF += NLAT_2;
V		BtF += NLAT_2;	BpF += NLAT_2;
	for(im=1; im<=imlim; ++im) {
		m = im*MRES;
		//l = LiM(shtns, 0,im);
		l = (im*(2*(LMAX+1)-(m+MRES)))>>1;
		//alm = shtns->alm[im];
		//alm = shtns->alm[0] + im*(2*LMAX - (im-1)*MRES);        // for m > 0
		alm += 2*(LMAX+1-m+MRES);

  #ifndef SHT_GRAD
V		SH_vect_to_2scal(shtns->mx_stdt + 2*l, llim, m, &Slm[l], &Tlm[l], (cplx*) VWl);
  #else
S		SHsph_to_2scal(shtns->mx_stdt + 2*l, llim, m, &Slm[l], (cplx*) VWl);
T		SHtor_to_2scal(shtns->mx_stdt + 2*l, llim, m, &Tlm[l], (cplx*) VWl);
  #endif

	#ifndef SHTNS_ISHIOKA
Q		cplx* Ql = &Qlm[l];	// virtual pointer for l=0 and im
	#else
		// pre-processing for recurrence relation of Ishioka
		const double* restrict xlm = shtns->xlm + 3*im*(2*(LMAX+4) -m+MRES)/4;
Q		v2d* Ql = (v2d*) &Qlm[l];	// virtual pointer for l=0 and im
Q		SH_to_ishioka(xlm, Ql+m, llim-m, QQl+m);
V		SH2_to_ishioka(xlm, VWl+2*m, llim-m+1);
	#endif

		k=0;	l=shtns->tm[im];
		l>>=1;		// stay on a 16 byte boundary
		while (k<l) {	// polar optimization
	#ifndef SHTNS4MAGIC
Q			BrF[k] = vdup(0.0);				BrF[(NPHI-2*im)*NLAT_2 + k] = vdup(0.0);
Q			BrF[NLAT_2-l+k] = vdup(0.0);	BrF[(NPHI+1-2*im)*NLAT_2 -l+k] = vdup(0.0);
V			BtF[k] = vdup(0.0);				BtF[(NPHI-2*im)*NLAT_2 + k] = vdup(0.0);
V			BtF[NLAT_2-l+k] = vdup(0.0);	BtF[(NPHI+1-2*im)*NLAT_2 -l+k] = vdup(0.0);
V			BpF[k] = vdup(0.0);				BpF[(NPHI-2*im)*NLAT_2 + k] = vdup(0.0);
V			BpF[NLAT_2-l+k] = vdup(0.0);	BpF[(NPHI+1-2*im)*NLAT_2 -l+k] = vdup(0.0);
	#else
Q			BrF[2*k] = vdup(0.0);			BrF[(NPHI-2*im)*NLAT_2 + 2*k] = vdup(0.0);
Q			BrF[2*k+1] = vdup(0.0);			BrF[(NPHI-2*im)*NLAT_2 +2*k+1] = vdup(0.0);
V			BtF[2*k] = vdup(0.0);			BtF[(NPHI-2*im)*NLAT_2 + 2*k] = vdup(0.0);
V			BtF[2*k+1] = vdup(0.0);			BtF[(NPHI-2*im)*NLAT_2 +2*k+1] = vdup(0.0);
V			BpF[2*k] = vdup(0.0);			BpF[(NPHI-2*im)*NLAT_2 + 2*k] = vdup(0.0);
V			BpF[2*k+1] = vdup(0.0);			BpF[(NPHI-2*im)*NLAT_2 +2*k+1] = vdup(0.0);
	#endif
			++k;
		}
		#ifdef _GCC_VEC_
		k = ((unsigned) k) / (VSIZE2/2);
		#else
		k *= 2;
		#endif
		do {
			al = alm;
			rnd cost[NWAY], y0[NWAY], y1[NWAY];
Q			rnd rer[NWAY], rei[NWAY], ror[NWAY], roi[NWAY];
V			rnd ter[NWAY], tei[NWAY], tor[NWAY], toi[NWAY];
V			rnd per[NWAY], pei[NWAY], por[NWAY], poi[NWAY];
			for (int j=0; j<NWAY; ++j) {
				cost[j] = vread(st, k+j);
				y0[j] = vall(1.0);
			}
			l=m;
V			if (robert_form == 0) l=m-1;
			long int ny = 0;
		  if ((int)llim <= SHT_L_RESCALE_FLY) {
			do {		// sin(theta)^m
				if (l&1) for (int j=0; j<NWAY; ++j) y0[j] *= cost[j];
				for (int j=0; j<NWAY; ++j) cost[j] *= cost[j];
			} while(l >>= 1);
		  } else {
			long int nsint = 0;
			do {		// sin(theta)^m		(use rescaling to avoid underflow)
				if (l&1) {
					for (int j=NWAY-1; j>=0; --j) y0[j] *= cost[j];
					ny += nsint;
					if (vlo(y0[NWAY-1]) < (SHT_ACCURACY+1.0/SHT_SCALE_FACTOR)) {
						ny--;
						for (int j=NWAY-1; j>=0; --j) y0[j] *= vall(SHT_SCALE_FACTOR);
					}
				}
				for (int j=NWAY-1; j>=0; --j) cost[j] *= cost[j];
				nsint += nsint;
				if (vlo(cost[NWAY-1]) < 1.0/SHT_SCALE_FACTOR) {
					nsint--;
					for (int j=NWAY-1; j>=0; --j) cost[j] *= vall(SHT_SCALE_FACTOR);
				}
			} while(l >>= 1);
		  }
			#ifdef SHTNS_ISHIOKA
			al = shtns->clm + im*(2*(LMAX+1) - m+MRES)/2;		// for ishioka recurrence
			#endif
			for (int j=0; j<NWAY; ++j) {
				cost[j] = vread(ct, j+k);
Q				ror[j] = vall(0.0);		roi[j] = vall(0.0);
Q				rer[j] = vall(0.0);		rei[j] = vall(0.0);
				#ifndef SHTNS_ISHIOKA
				y0[j] *= vall(al[0]);
				#else
				cost[j] *= cost[j];		// cos(theta)^2
				#endif
			}
			for (int j=0; j<NWAY; ++j) {
				#ifndef SHTNS_ISHIOKA
				y1[j]  = (vall(al[1])*y0[j]) *cost[j];		//	y1[j] = vall(al[1])*cost[j]*y0[j];
				#else
				y1[j] = (vall(al[1])*cost[j] + vall(al[0]))*y0[j];
				#endif
V				por[j] = vall(0.0);		tei[j] = vall(0.0);
V				tor[j] = vall(0.0);		pei[j] = vall(0.0);
V				poi[j] = vall(0.0);		ter[j] = vall(0.0);
V				toi[j] = vall(0.0);		per[j] = vall(0.0);
			}
			l=m;		al+=2;
			while ((ny<0) && (l<llim)) {		// ylm treated as zero and ignored if ny < 0
				#ifndef SHTNS_ISHIOKA
				for (int j=0; j<NWAY; ++j) {
					y0[j] = (vall(al[1])*cost[j])*y1[j] + vall(al[0])*y0[j];
				}
				for (int j=0; j<NWAY; ++j) {
					y1[j] = (vall(al[3])*cost[j])*y0[j] + vall(al[2])*y1[j];
				}
				al+=4;
				#else
				for (int j=0; j<NWAY; ++j) {
					rnd tmp = y1[j];
					y1[j] = (vall(al[1])*cost[j] + vall(al[0]))*y1[j] + y0[j];
					y0[j] = tmp;
				}
				al+=2;
				#endif
				l+=2;
				if (fabs(vlo(y0[NWAY-1])) > SHT_ACCURACY*SHT_SCALE_FACTOR + 1.0) {		// rescale when value is significant
					++ny;
					for (int j=0; j<NWAY; ++j) {
						y0[j] *= vall(1.0/SHT_SCALE_FACTOR);		y1[j] *= vall(1.0/SHT_SCALE_FACTOR);
					}
				}
			}
		  if (ny == 0) {
		#ifndef SHTNS_ISHIOKA
			while (l<llim) {	// compute even and odd parts
Q				for (int j=0; j<NWAY; ++j) {	rer[j] += y0[j]  * qr(l);		rei[j] += y0[j] * qi(l);	}
V				for (int j=0; j<NWAY; ++j) {	ter[j] += y0[j]  * vr(l);		tei[j] += y0[j] * vi(l);	}
V				for (int j=0; j<NWAY; ++j) {	per[j] += y0[j]  * wr(l);		pei[j] += y0[j] * wi(l);	}
				for (int j=0; j<NWAY; ++j) {
					y0[j] = vall(al[1])*(cost[j]*y1[j]) + vall(al[0])*y0[j];
				}
Q				for (int j=0; j<NWAY; ++j) {	ror[j] += y1[j]  * qr(l+1);		roi[j] += y1[j] * qi(l+1);	}
V				for (int j=0; j<NWAY; ++j) {	tor[j] += y1[j]  * vr(l+1);		toi[j] += y1[j] * vi(l+1);	}
V				for (int j=0; j<NWAY; ++j) {	por[j] += y1[j]  * wr(l+1);		poi[j] += y1[j] * wi(l+1);	}
				for (int j=0; j<NWAY; ++j) {
					y1[j] = vall(al[3])*(cost[j]*y0[j]) + vall(al[2])*y1[j];
				}
				l+=2;	al+=4;
			}
V				for (int j=0; j<NWAY; ++j) {	ter[j] += y0[j]  * vr(l);		tei[j] += y0[j] * vi(l);	}
V				for (int j=0; j<NWAY; ++j) {	per[j] += y0[j]  * wr(l);		pei[j] += y0[j] * wi(l);	}
			if (l==llim) {
Q				for (int j=0; j<NWAY; ++j) {	rer[j] += y0[j]  * qr(l);		rei[j] += y0[j] * qi(l);	}
V				for (int j=0; j<NWAY; ++j) {	tor[j] += y1[j]  * vr(l+1);		toi[j] += y1[j] * vi(l+1);	}
V				for (int j=0; j<NWAY; ++j) {	por[j] += y1[j]  * wr(l+1);		poi[j] += y1[j] * wi(l+1);	}
			}
		#else
			while (l<llim) {	// compute even and odd parts
Q				for (int j=0; j<NWAY; ++j) {	rer[j] += y0[j]  * qr(l);		rei[j] += y0[j] * qi(l);	}
Q				for (int j=0; j<NWAY; ++j) {	ror[j] += y0[j]  * qr(l+1);		roi[j] += y0[j] * qi(l+1);	}
V				for (int j=0; j<NWAY; ++j) {	ter[j] += y0[j]  * vr(l);		tei[j] += y0[j] * vi(l);	}
V				for (int j=0; j<NWAY; ++j) {	per[j] += y0[j]  * wr(l);		pei[j] += y0[j] * wi(l);	}
V				for (int j=0; j<NWAY; ++j) {	tor[j] += y0[j]  * vr(l+1);		toi[j] += y0[j] * vi(l+1);	}
V				for (int j=0; j<NWAY; ++j) {	por[j] += y0[j]  * wr(l+1);		poi[j] += y0[j] * wi(l+1);	}
				for (int j=0; j<NWAY; ++j) {
					rnd tmp = y1[j];
					y1[j] = (vall(al[1])*cost[j] + vall(al[0]))*y1[j] + y0[j];
					y0[j] = tmp;
				}
				l+=2;	al+=2;
			}
V				for (int j=0; j<NWAY; ++j) {	ter[j] += y0[j]  * vr(l);		tei[j] += y0[j] * vi(l);	}
V				for (int j=0; j<NWAY; ++j) {	per[j] += y0[j]  * wr(l);		pei[j] += y0[j] * wi(l);	}
			if (l==llim) {
V				for (int j=0; j<NWAY; ++j) {	tor[j] += y0[j]  * vr(l+1);		toi[j] += y0[j] * vi(l+1);	}
V				for (int j=0; j<NWAY; ++j) {	por[j] += y0[j]  * wr(l+1);		poi[j] += y0[j] * wi(l+1);	}
Q				for (int j=0; j<NWAY; ++j) {	rer[j] += y0[j]  * qr(l);		rei[j] += y0[j] * qi(l);	}
			}
			// correct the odd part:
			for (int j=0; j<NWAY; ++j) cost[j] = vread(ct, k+j);
V			for (int j=0; j<NWAY; ++j) {  tor[j] *= cost[j];	toi[j] *= cost[j]; }
V			for (int j=0; j<NWAY; ++j) {  por[j] *= cost[j];	poi[j] *= cost[j]; }
Q			for (int j=0; j<NWAY; ++j) {  ror[j] *= cost[j];	roi[j] *= cost[j]; }
		#endif
3			if (robert_form == 0) {
3				for (int j=0; j<NWAY; ++j) cost[j]  = vread(st, k+j);
3				for (int j=0; j<NWAY; ++j) {  rer[j] *= cost[j];  ror[j] *= cost[j];	rei[j] *= cost[j];  roi[j] *= cost[j];  }
3			}
		  }
		#ifndef SHTNS4MAGIC
		  #ifdef _GCC_VEC_
			for (int j=0; j<NWAY; ++j) {
Q				S2D_CSTORE(BrF, k+j, rer[j], ror[j], rei[j], roi[j])
V				S2D_CSTORE(BtF, k+j, ter[j], tor[j], tei[j], toi[j])
V				S2D_CSTORE(BpF, k+j, per[j], por[j], pei[j], poi[j])
			}
		  #else
			for (int j=0; j<NWAY/2; ++j) {		// NWAY is even when _GCC_VEC_ is not defined
Q				S2D_CSTOREX(BrF, k/2+j, 2*j, rer, ror, rei, roi)
V				S2D_CSTOREX(BtF, k/2+j, 2*j, ter, tor, tei, toi)
V				S2D_CSTOREX(BpF, k/2+j, 2*j, per, por, pei, poi)
			}
		  #endif
		#else
			for (int j=0; j<NWAY; ++j) {
				if ((k+j)>=nk) break;
Q				S2D_CSTORE_4MAGIC((double*) BrF, (double*) (BrF + (NPHI-2*im)*NLAT_2), k+j, rer[j], ror[j], rei[j], roi[j]);
V				S2D_CSTORE_4MAGIC((double*) BtF, (double*) (BtF + (NPHI-2*im)*NLAT_2), k+j, ter[j], tor[j], tei[j], toi[j]);
V				S2D_CSTORE_4MAGIC((double*) BpF, (double*) (BpF + (NPHI-2*im)*NLAT_2), k+j, per[j], por[j], pei[j], poi[j]);
			}
		#endif
			k+=NWAY;
		} while (k < nk);
Q		BrF += NLAT_2;
V		BtF += NLAT_2;	BpF += NLAT_2;
	}

	for (k=0; k < NLAT_2*(NPHI-1-2*imlim); ++k) {	// padding for high m's
Q		BrF[k] = vdup(0.0);
V		BtF[k] = vdup(0.0);	BpF[k] = vdup(0.0);
	}
Q	BrF -= NLAT_2*(imlim+1);		// restore original pointer
V	BtF -= NLAT_2*(imlim+1);	BpF -= NLAT_2*(imlim+1);
    // NPHI > 1 as SHT_AXISYM is not defined.
  	if (shtns->fftc_mode >= 0) {
		if (shtns->fftc_mode != 1) {
Q			fftw_execute_dft(shtns->ifftc, (cplx *) BrF, (cplx *) Vr);
V			fftw_execute_dft(shtns->ifftc, (cplx *) BtF, (cplx *) Vt);
V			fftw_execute_dft(shtns->ifftc, (cplx *) BpF, (cplx *) Vp);
		} else {		// split dft
Q			fftw_execute_split_dft(shtns->ifftc,((double*)BrF)+1, ((double*)BrF), Vr+NPHI, Vr);
V			fftw_execute_split_dft(shtns->ifftc,((double*)BtF)+1, ((double*)BtF), Vt+NPHI, Vt);
V			fftw_execute_split_dft(shtns->ifftc,((double*)BpF)+1, ((double*)BpF), Vp+NPHI, Vp);
Q			VFREE(BrF);
VX			VFREE(BtF);		// this frees also BpF.
		}
	}
  #endif

Q	#undef qr
Q	#undef qi
S	#undef sr
S	#undef si
T	#undef tr
T	#undef ti
  }

  #ifndef SHT_AXISYM

3	static void GEN3(SHqst_m_to_spat_fly,NWAY,SUFFIX)(shtns_cfg shtns, int im, cplx *Qlm, cplx *Slm, cplx *Tlm, cplx *Vr, cplx *Vt, cplx *Vp, const long int llim) {
QX	static void GEN3(SH_m_to_spat_fly,NWAY,SUFFIX)(shtns_cfg shtns, int im, cplx *Qlm, cplx *Vr, const long int llim) {
  #ifndef SHT_GRAD
VX	static void GEN3(SHsphtor_m_to_spat_fly,NWAY,SUFFIX)(shtns_cfg shtns, int im, cplx *Slm, cplx *Tlm, cplx *Vt, cplx *Vp, const long int llim) {
  #else
S	static void GEN3(SHsph_m_to_spat_fly,NWAY,SUFFIX)(shtns_cfg shtns, int im, cplx *Slm, cplx *Vt, cplx *Vp, const long int llim) {
T	static void GEN3(SHtor_m_to_spat_fly,NWAY,SUFFIX)(shtns_cfg shtns, int im, cplx *Tlm, cplx *Vt, cplx *Vp, const long int llim) {
  #endif

Q	v2d *BrF;
V	v2d *BtF, *BpF;
Q	#define qr(l) vall(creal(Qlm[l]))
Q	#define qi(l) vall(cimag(Qlm[l]))
V	#define vr(l) vall( ((double*) VWl)[4*(l)]   )
V	#define vi(l) vall( ((double*) VWl)[4*(l)+1] )
V	#define wr(l) vall( ((double*) VWl)[4*(l)+2] )
V	#define wi(l) vall( ((double*) VWl)[4*(l)+3] )
	long int nk, k,l,m;
	double *alm, *al;
	s2d *ct, *st;
V	int robert_form;

Q	BrF = (v2d*) Vr;
V	BtF = (v2d*) Vt;	BpF = (v2d*) Vp;

	nk = NLAT_2;
	#if _GCC_VEC_
		nk = ((unsigned)(nk+VSIZE2-1)) / VSIZE2;
	#endif
	ct = (s2d*) shtns->ct;		st = (s2d*) shtns->st;
V	robert_form = shtns->robert_form;

	if (im == 0) {
Q		double Ql0[llim+1];
S		double Sl0[llim];
T		double Tl0[llim];

		#ifdef SHT_GRAD
S			k=0; do { BpF[k]=vdup(0.0); } while(++k<NLAT);
T			k=0; do { BtF[k]=vdup(0.0); } while(++k<NLAT);
		#endif

 		l=1;
		alm = shtns->alm;
Q		Ql0[0] = (double) Qlm[0];		// l=0
		do {		// for m=0, compress the complex Q,S,T to double
Q			Ql0[l] = (double) Qlm[l];	//	Ql[l+1] = (double) Qlm[l+1];
S			Sl0[l-1] = (double) Slm[l];	//	Sl[l] = (double) Slm[l+1];
T			Tl0[l-1] = (double) Tlm[l];	//	Tl[l] = (double) Tlm[l+1];
			++l;
		} while(l<=llim);
		k=0;
		do {
			l=0;	al = alm;
			rnd cost[NWAY], y0[NWAY], y1[NWAY];
V			rnd sint[NWAY], dy0[NWAY], dy1[NWAY];
Q			rnd re[NWAY], ro[NWAY];
S			rnd te[NWAY], to[NWAY];
T			rnd pe[NWAY], po[NWAY];
			for (int j=0; j<NWAY; ++j) {
				cost[j] = vread(ct, j+k);
V				sint[j] = -vread(st, j+k);
				y0[j] = vall(al[0]);
V				dy0[j] = vall(0.0);
Q				re[j] = y0[j] * vall(Ql0[0]);
S				to[j] = dy0[j];
T				po[j] = dy0[j];
			}
V			if (robert_form) {
V				for (int j=0; j<NWAY; ++j) sint[j] *= -sint[j];
V			}
			for (int j=0; j<NWAY; ++j) {
				y1[j]  = vall(al[0]*al[1]) * cost[j];
V				dy1[j] = vall(al[0]*al[1]) * sint[j];
			}
			for (int j=0; j<NWAY; ++j) {
Q				ro[j] = y1[j] * vall(Ql0[1]);
S				te[j] = dy1[j] * vall(Sl0[0]);
T				pe[j] = -dy1[j] * vall(Tl0[0]);
			}
			al+=2;	l+=2;
			while(l<llim) {
				for (int j=0; j<NWAY; ++j) {
V					dy0[j] = vall(al[1])*(cost[j]*dy1[j] + y1[j]*sint[j]) + vall(al[0])*dy0[j];
					y0[j]  = vall(al[1])*(cost[j]*y1[j]) + vall(al[0])*y0[j];
				}
				for (int j=0; j<NWAY; ++j) {
Q					re[j] += y0[j] * vall(Ql0[l]);
S					to[j] += dy0[j] * vall(Sl0[l-1]);
T					po[j] -= dy0[j] * vall(Tl0[l-1]);
				}
				for (int j=0; j<NWAY; ++j) {
V					dy1[j] = vall(al[3])*(cost[j]*dy0[j] + y0[j]*sint[j]) + vall(al[2])*dy1[j];
					y1[j]  = vall(al[3])*(cost[j]*y0[j]) + vall(al[2])*y1[j];
				}
				for (int j=0; j<NWAY; ++j) {
Q					ro[j] += y1[j] * vall(Ql0[l+1]);
S					te[j] += dy1[j] * vall(Sl0[l]);
T					pe[j] -= dy1[j] * vall(Tl0[l]);
				}
				al+=4;	l+=2;
			}
			if (l==llim) {
				for (int j=0; j<NWAY; ++j) {
V					dy0[j] = vall(al[1])*(cost[j]*dy1[j] + y1[j]*sint[j]) + vall(al[0])*dy0[j];
					y0[j]  = vall(al[1])*cost[j]*y1[j] + vall(al[0])*y0[j];
				}
				for (int j=0; j<NWAY; ++j) {
Q					re[j] += y0[j] * vall(Ql0[l]);
S					to[j] += dy0[j] * vall(Sl0[l-1]);
T					po[j] -= dy0[j] * vall(Tl0[l-1]);
				}
			}
		#ifndef SHTNS4MAGIC
			for (int j=0; j<NWAY; ++j) {
Q				S2D_CSTORE2(BrF, k+j, re[j], ro[j], vall(0), vall(0))
S				S2D_CSTORE2(BtF, k+j, te[j], to[j], vall(0), vall(0))
T				S2D_CSTORE2(BpF, k+j, pe[j], po[j], vall(0), vall(0))
			}
		#else
			for (int j=0; j<NWAY; ++j) {
				if ((k+j)>=nk) break;
Q				S2D_CSTORE2_4MAGIC((double*)BrF, k+j, re[j], ro[j], vall(0), vall(0));
S				S2D_CSTORE2_4MAGIC((double*)BtF, k+j, te[j], to[j], vall(0), vall(0));
T				S2D_CSTORE2_4MAGIC((double*)BpF, k+j, pe[j], po[j], vall(0), vall(0));
			}
		#endif
			k+=NWAY;
		} while (k < nk);

	} else {	// im > 0
V		const int ms = im*MRES;		// signed m.
		im = abs(im);			// positive im
V		v2d VWl[llim*2+4];
		m = im*MRES;
		l = im*(2*(LMAX+1) -m);		// to compute position in NLM array.
		alm = shtns->alm + l+m;
Q		Qlm -= m;	// virtual pointer for l=0
S		Slm -= m;	// virtual pointer for l=0
T		Tlm -= m;

V		{	// convert from vector SH to scalar SH
V			double* mx = shtns->mx_stdt + l-m;
V			s2d em = vdup(ms);
S			v2d sl = ((v2d*)Slm)[m];
T			v2d tl = ((v2d*)Tlm)[m];
V			v2d vs = vdup( 0.0 );
V			v2d wt = vdup( 0.0 );
V			for (int l=m; l<=llim; l++) {
V				s2d mxu = vdup( mx[2*l] );
V				s2d mxl = vdup( mx[2*l+1] );		// mxl for next iteration
T				vs = addi( vs ,  em*tl );
S				wt = addi( wt ,  em*sl );
S				v2d vs1 = mxl*sl;			// vs for next iter
T				v2d wt1 = -mxl*tl;			// wt for next iter
V				if (l<llim) {
S					sl = ((v2d*)Slm)[l+1];		// kept for next iteration
T					tl = ((v2d*)Tlm)[l+1];
S					vs += mxu*sl;
T					wt -= mxu*tl;
V				}
V				VWl[2*l]   = vs;
V				VWl[2*l+1] = wt;
V				vs = vdup( 0.0 );		wt = vdup( 0.0 );
S				vs = vs1;
T				wt = wt1;
V			}
V			VWl[2*llim+2] = vs;
V			VWl[2*llim+3] = wt;
V		}

		k=0;	l=shtns->tm[im];
		while (k<l) {	// polar optimization
		  #ifndef SHTNS4MAGIC
Q			BrF[k] = vdup(0.0);		BrF[NLAT-l+k] = vdup(0.0);
V			BtF[k] = vdup(0.0);		BtF[NLAT-l+k] = vdup(0.0);
V			BpF[k] = vdup(0.0);		BpF[NLAT-l+k] = vdup(0.0);
		  #else
Q			BrF[2*k] = vdup(0.0);		BrF[2*k+1] = vdup(0.0);
V			BtF[2*k] = vdup(0.0);		BtF[2*k+1] = vdup(0.0);
V			BpF[2*k] = vdup(0.0);		BpF[2*k+1] = vdup(0.0);
		  #endif
			++k;
		}

		k = ((unsigned) l) / VSIZE2;
		do {
			al = alm;
			rnd cost[NWAY], y0[NWAY], y1[NWAY];
Q			rnd rer[NWAY], rei[NWAY], ror[NWAY], roi[NWAY];
V			rnd ter[NWAY], tei[NWAY], tor[NWAY], toi[NWAY];
V			rnd per[NWAY], pei[NWAY], por[NWAY], poi[NWAY];
			for (int j=0; j<NWAY; ++j) {
				cost[j] = vread(st, k+j);
				y0[j] = vall(1.0);
			}
			l=m;
V			if (robert_form == 0) l=m-1;
			long int ny = 0;
		  if ((int)llim <= SHT_L_RESCALE_FLY) {
			do {		// sin(theta)^m
				if (l&1) for (int j=0; j<NWAY; ++j) y0[j] *= cost[j];
				for (int j=0; j<NWAY; ++j) cost[j] *= cost[j];
			} while(l >>= 1);
		  } else {
			long int nsint = 0;
			do {		// sin(theta)^m		(use rescaling to avoid underflow)
				if (l&1) {
					for (int j=NWAY-1; j>=0; --j) y0[j] *= cost[j];
					ny += nsint;
					if (vlo(y0[NWAY-1]) < (SHT_ACCURACY+1.0/SHT_SCALE_FACTOR)) {
						ny--;
						for (int j=NWAY-1; j>=0; --j) y0[j] *= vall(SHT_SCALE_FACTOR);
					}
				}
				for (int j=NWAY-1; j>=0; --j) cost[j] *= cost[j];
				nsint += nsint;
				if (vlo(cost[NWAY-1]) < 1.0/SHT_SCALE_FACTOR) {
					nsint--;
					for (int j=NWAY-1; j>=0; --j) cost[j] *= vall(SHT_SCALE_FACTOR);
				}
			} while(l >>= 1);
		  }
			for (int j=0; j<NWAY; ++j) {
				y0[j] *= vall(al[0]);
				cost[j] = vread(ct, j+k);
Q				ror[j] = vall(0.0);		roi[j] = vall(0.0);
Q				rer[j] = vall(0.0);		rei[j] = vall(0.0);
			}
			for (int j=0; j<NWAY; ++j) {
				y1[j]  = (vall(al[1])*y0[j]) *cost[j];		//	y1[j] = vall(al[1])*cost[j]*y0[j];
V				por[j] = vall(0.0);		tei[j] = vall(0.0);
V				tor[j] = vall(0.0);		pei[j] = vall(0.0);
V				poi[j] = vall(0.0);		ter[j] = vall(0.0);
V				toi[j] = vall(0.0);		per[j] = vall(0.0);
			}
			l=m;		al+=2;
			while ((ny<0) && (l<llim)) {		// ylm treated as zero and ignored if ny < 0
				for (int j=0; j<NWAY; ++j) {
					y0[j] = (vall(al[1])*cost[j])*y1[j] + vall(al[0])*y0[j];
				}
				for (int j=0; j<NWAY; ++j) {
					y1[j] = (vall(al[3])*cost[j])*y0[j] + vall(al[2])*y1[j];
				}
				l+=2;	al+=4;
				if (fabs(vlo(y0[NWAY-1])) > SHT_ACCURACY*SHT_SCALE_FACTOR + 1.0) {		// rescale when value is significant
					++ny;
					for (int j=0; j<NWAY; ++j) {
						y0[j] *= vall(1.0/SHT_SCALE_FACTOR);		y1[j] *= vall(1.0/SHT_SCALE_FACTOR);
					}
				}
			}
		  if (ny == 0) {
			while (l<llim) {	// compute even and odd parts
Q				for (int j=0; j<NWAY; ++j) {	rer[j] += y0[j]  * qr(l);		rei[j] += y0[j] * qi(l);	}
V				for (int j=0; j<NWAY; ++j) {	ter[j] += y0[j]  * vr(l);		tei[j] += y0[j] * vi(l);	}
V				for (int j=0; j<NWAY; ++j) {	per[j] += y0[j]  * wr(l);		pei[j] += y0[j] * wi(l);	}
				for (int j=0; j<NWAY; ++j) {
					y0[j] = vall(al[1])*(cost[j]*y1[j]) + vall(al[0])*y0[j];
				}
Q				for (int j=0; j<NWAY; ++j) {	ror[j] += y1[j]  * qr(l+1);		roi[j] += y1[j] * qi(l+1);	}
V				for (int j=0; j<NWAY; ++j) {	tor[j] += y1[j]  * vr(l+1);		toi[j] += y1[j] * vi(l+1);	}
V				for (int j=0; j<NWAY; ++j) {	por[j] += y1[j]  * wr(l+1);		poi[j] += y1[j] * wi(l+1);	}
				for (int j=0; j<NWAY; ++j) {
					y1[j] = vall(al[3])*(cost[j]*y0[j]) + vall(al[2])*y1[j];
				}
				l+=2;	al+=4;
			}
V				for (int j=0; j<NWAY; ++j) {	ter[j] += y0[j]  * vr(l);		tei[j] += y0[j] * vi(l);	}
V				for (int j=0; j<NWAY; ++j) {	per[j] += y0[j]  * wr(l);		pei[j] += y0[j] * wi(l);	}
			if (l==llim) {
Q				for (int j=0; j<NWAY; ++j) {	rer[j] += y0[j]  * qr(l);		rei[j] += y0[j] * qi(l);	}
V				for (int j=0; j<NWAY; ++j) {	tor[j] += y1[j]  * vr(l+1);		toi[j] += y1[j] * vi(l+1);	}
V				for (int j=0; j<NWAY; ++j) {	por[j] += y1[j]  * wr(l+1);		poi[j] += y1[j] * wi(l+1);	}
			}
3			if (robert_form == 0) {
3				for (int j=0; j<NWAY; ++j) cost[j]  = vread(st, k+j);
3				for (int j=0; j<NWAY; ++j) {  rer[j] *= cost[j];  ror[j] *= cost[j];	rei[j] *= cost[j];  roi[j] *= cost[j];  }
3			}
		  }
		#ifndef SHTNS4MAGIC
			for (int j=0; j<NWAY; ++j) {
Q				S2D_CSTORE2(BrF, k+j, rer[j], ror[j], rei[j], roi[j])
V				S2D_CSTORE2(BtF, k+j, ter[j], tor[j], tei[j], toi[j])
V				S2D_CSTORE2(BpF, k+j, per[j], por[j], pei[j], poi[j])
			}
		#else
			for (int j=0; j<NWAY; ++j) {
				if ((k+j)>=nk) break;
Q				S2D_CSTORE2_4MAGIC((double*)BrF, k+j, rer[j], ror[j], rei[j], roi[j]);
V				S2D_CSTORE2_4MAGIC((double*)BtF, k+j, ter[j], tor[j], tei[j], toi[j]);
V				S2D_CSTORE2_4MAGIC((double*)BpF, k+j, per[j], por[j], pei[j], poi[j]);
			}
		#endif
			k+=NWAY;
		} while (k < nk);
	}

Q	#undef qr
Q	#undef qi
S	#undef sr
S	#undef si
T	#undef tr
T	#undef ti
  }

  #endif
