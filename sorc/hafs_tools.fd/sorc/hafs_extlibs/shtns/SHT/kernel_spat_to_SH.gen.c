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
# it is intended for "make" to generate C code for 3 similar SHT functions,
# (namely spat_to_SH [Q tag]), spat_to_SHsphtor [V tag], spat_to_SH3 [both Q&V tags])
# from one generic function + tags.
# Basically, there are tags at the beginning of lines (Q,V) that are information
# to keep or remove the line depending on the function to build. (Q for scalar, V for vector, # for comment)
#
//////////////////////////////////////////////////

	#ifdef HI_LLIM
QX	#define BASE _an1_hi
VX	#define BASE _an2_hi
3	#define BASE _an3_hi
	#else
QX	#define BASE _an1
VX	#define BASE _an2
3	#define BASE _an3
	#endif

QX	void GEN3(BASE,NWAY,SUFFIX)(shtns_cfg shtns, double *BrF, cplx *Qlm, const long int llim, const int imlim)
VX	void GEN3(BASE,NWAY,SUFFIX)(shtns_cfg shtns, double *BtF, double *BpF, cplx *Slm, cplx *Tlm, const long int llim, const int imlim)
3	void GEN3(BASE,NWAY,SUFFIX)(shtns_cfg shtns, double *BrF, double *BtF, double *BpF, cplx *Qlm, cplx *Slm, cplx *Tlm, const long int llim, const int imlim)
  {
	// TODO: NW should be larger for SHTNS_ISHIOKA ?, or take a different approach.
	#define NW (NWAY*16)
	// another blocking level in theta (64 or 128 should be good)
	#define NBLK (NWAY*16) //24 //(NWAY*3) //24 //96 // (NWAY*3)  //24
	// LSPAN can be 2 or 4
QX	#define LSPAN 4
V	#define LSPAN 2

	double *alm, *al;
	double *wg, *ct, *st;
V	double *l_2;
	long int nk, k, l,m;
	unsigned m0, mstep;
  #ifndef SHT_AXISYM
	unsigned im;
  #endif
V	int robert_form;
Q	v2d qq[llim+2];
V	v2d vw[2*llim+4];

	// the SSE macro should align these arrays on vector length.
Q	double rer[NLAT_2 + NW*VSIZE2] SSE;
Q	double ror[NLAT_2 + NW*VSIZE2] SSE;
V	double ter[NLAT_2 + NW*VSIZE2] SSE;
V	double tor[NLAT_2 + NW*VSIZE2] SSE;
V	double per[NLAT_2 + NW*VSIZE2] SSE;
V	double por[NLAT_2 + NW*VSIZE2] SSE;
  #ifndef SHT_AXISYM
Q	double rei[NLAT_2 + NW*VSIZE2] SSE;
Q	double roi[NLAT_2 + NW*VSIZE2] SSE;
V	double tei[NLAT_2 + NW*VSIZE2] SSE;
V	double toi[NLAT_2 + NW*VSIZE2] SSE;
V	double pei[NLAT_2 + NW*VSIZE2] SSE;
V	double poi[NLAT_2 + NW*VSIZE2] SSE;
  #endif

	nk = NLAT_2;	// copy NLAT_2 to a local variable for faster access (inner loop limit)
	#if _GCC_VEC_
	  nk = ((unsigned) nk+(VSIZE2-1))/VSIZE2;
	#endif
	wg = shtns->wg;		ct = shtns->ct;		st = shtns->st;
V	robert_form = shtns->robert_form;
V	l_2 = shtns->l_2;
	for (k=nk*VSIZE2; k<(nk-1+NW)*VSIZE2; ++k) {		// never written, so this is now done for all m's
Q		rer[k] = 0.0;		ror[k] = 0.0;
V		ter[k] = 0.0;		tor[k] = 0.0;
V		per[k] = 0.0;		por[k] = 0.0;
	  #ifndef SHT_AXISYM
Q		rei[k] = 0.0;		roi[k] = 0.0;
V		tei[k] = 0.0;		toi[k] = 0.0;
V		pei[k] = 0.0;		poi[k] = 0.0;
	  #endif
	}

	// ACCESS PATTERN
	const int k_inc = shtns->k_stride_a;
	const int m_inc = shtns->m_stride_a;

	#ifndef _OPENMP
		m0 = 0;		mstep = 1;
	#else
		m0 = omp_get_thread_num();
		mstep = omp_get_num_threads();
		if (m0 == 0)
	#endif
	{		// im=0 : dzl.p = 0.0 and evrything is REAL
		alm = shtns->blm;
Q		double r0 = 0.0;
		// compute symmetric and antisymmetric parts. (do not weight here, it is cheaper to weight y0)
Q		SYM_ASYM_M0_Q(BrF, rer, ror, r0)
V		SYM_ASYM_M0_V(BtF, ter, tor)
V		SYM_ASYM_M0_V(BpF, per, por)
Q		Qlm[0] = r0 * alm[0];			// l=0 is done.
V		Slm[0] = 0.0;		Tlm[0] = 0.0;		// l=0 is zero for the vector transform.
		k = 0;
Q		double* q_ = (double*) qq;
V		double* v_ = (double*) vw;
		for (l=0;l<llim;++l) {
Q			q_[l] = 0.0;
V			v_[2*l] = 0.0;		v_[2*l+1] = 0.0;
		}
		do {
			al = alm;
			rnd cost[NW], y0[NW], y1[NW];
V			rnd sint[NW], dy0[NW], dy1[NW];
Q			rnd rerk[NW], rork[NW];		// help the compiler to cache into registers.
V			rnd terk[NW], tork[NW], perk[NW], pork[NW];
			const int blk_sze = (k+NW <= nk) ? NW : nk-k;		// limit block size to avoid reading garbage as arrays overflow
			for (int j=0; j<blk_sze; ++j) {
				cost[j] = vread(ct, k+j);
				y0[j] = vall(al[0]) * vread(wg, k+j);		// weight of Gauss quadrature appears here
V				dy0[j] = vall(0.0);
V				sint[j] = -vread(st, k+j);
				y1[j] =  (vall(al[1])*y0[j]) * cost[j];
V				dy1[j] = (vall(al[1])*y0[j]) * sint[j];
Q				rerk[j] = vread(rer, k+j);		rork[j] = vread(ror, k+j);		// cache into registers.
V				terk[j] = vread(ter, k+j);		tork[j] = vread(tor, k+j);
V				perk[j] = vread(per, k+j);		pork[j] = vread(por, k+j);
			}
V			if (robert_form) {
V				for (int j=0; j<blk_sze; ++j) {
V					rnd st_1 = vread(shtns->st_1, k+j);
V					terk[j] *= st_1;	tork[j] *= st_1;
V					perk[j] *= st_1;	pork[j] *= st_1;
V				}
V			}
			al+=2;	l=1;
			while(l<llim) {
				for (int j=0; j<blk_sze; ++j) {
V					dy0[j] = vall(al[1])*(cost[j]*dy1[j] + y1[j]*sint[j]) + vall(al[0])*dy0[j];
					y0[j]  = vall(al[1])*(cost[j]*y1[j]) + vall(al[0])*y0[j];
				}
Q				rnd q = y1[0] * rork[0];
V				rnd s = dy1[0] * terk[0];
V				rnd t = dy1[0] * perk[0];
				for (int j=1; j<blk_sze; ++j) {
Q					q += y1[j] * rork[j];
V					s += dy1[j] * terk[j];
V					t += dy1[j] * perk[j];
				}
Q				q_[l-1]   += reduce_add(q);
V				v_[2*l-2] += reduce_add(s);
V				v_[2*l-1] -= reduce_add(t);
				for (int j=0; j<blk_sze; ++j) {
V					dy1[j] = vall(al[3])*(cost[j]*dy0[j] + y0[j]*sint[j]) + vall(al[2])*dy1[j];
					y1[j]  = vall(al[3])*(cost[j]*y0[j]) + vall(al[2])*y1[j];
				}
Q				q = y0[0] * rerk[0];
V				s = dy0[0] * tork[0];
V				t = dy0[0] * pork[0];
				for (int j=1; j<blk_sze; ++j) {
Q					q += y0[j] * rerk[j];
V					s += dy0[j] * tork[j];
V					t += dy0[j] * pork[j];
				}
Q				q_[l]     += reduce_add(q);
V				v_[2*l]   += reduce_add(s);
V				v_[2*l+1] -= reduce_add(t);
				al+=4;	l+=2;
			}
			if (l==llim) {
Q				rnd q = y1[0] * rork[0];
V				rnd s = dy1[0] * terk[0];
V				rnd t = dy1[0] * perk[0];
				for (int j=1; j<blk_sze; ++j) {
Q					q += y1[j] * rork[j];
V					s += dy1[j] * terk[j];
V					t += dy1[j] * perk[j];
				}
Q				q_[l-1]   += reduce_add(q);
V				v_[2*l-2] += reduce_add(s);
V				v_[2*l-1] -= reduce_add(t);
			}
			k+=NW;
		} while (k < nk);
		for (l=1; l<=llim; ++l) {
Q			Qlm[l] = q_[l-1];
V			Slm[l] = v_[2*l-2]*l_2[l];		Tlm[l] = v_[2*l-1]*l_2[l];
		}
		#ifdef SHT_VAR_LTR
			for (l=llim+1; l<= LMAX; ++l) {
Q				((v2d*)Qlm)[l] = vdup(0.0);
V				((v2d*)Slm)[l] = vdup(0.0);		((v2d*)Tlm)[l] = vdup(0.0);
			}
			#ifndef SHT_AXISYM
			if (imlim <= MMAX) {		// zero out m >= imlim
				l = LiM(shtns, imlim*MRES, imlim);
				do {
Q					((v2d*)Qlm)[l] = vdup(0.0);
V					((v2d*)Slm)[l] = vdup(0.0);		((v2d*)Tlm)[l] = vdup(0.0);
				} while(++l < shtns->nlm);
			}
			#endif
		#endif
		m0=mstep;
	}

  #ifndef SHT_AXISYM
	const double mpos_scale = shtns->mpos_scale_analys;		// handles real-norm
	for (im=m0; im<imlim; im+=mstep) {
		m = im*MRES;
		int k0 = shtns->tm[im] / VSIZE2;
		#ifndef SHTNS_ISHIOKA
		alm = shtns->blm + im*(2*(LMAX+1) -m+MRES);
		#else
		alm = shtns->clm + im*(2*(LMAX+1) - m+MRES)/2;
		#endif
		// compute symmetric and anti-symmetric parts:
	#ifndef SHTNS_ISHIOKA
QX		SYM_ASYM_Q(BrF, rer, ror, rei, roi, k0)
3		SYM_ASYM_Q3(BrF, rer, ror, rei, roi, k0)
V		SYM_ASYM_V(BtF, ter, tor, tei, toi, k0)
V		SYM_ASYM_V(BpF, per, por, pei, poi, k0)
	#else
QX		SYM_ASYM_Q_ISH(BrF, rer, ror, rei, roi, k0)
3		SYM_ASYM_Q3_ISH(BrF, rer, ror, rei, roi, k0)
V		SYM_ASYM_V_ISH(BtF, ter, tor, tei, toi, k0)
V		SYM_ASYM_V_ISH(BpF, per, por, pei, poi, k0)
	#endif
	
		for (int l=0; l<=llim-m+1; l++) {
Q			qq[l] = vdup(0.0);
V			vw[2*l] = vdup(0.0);		vw[2*l+1] = vdup(0.0);
		}
		int k = nk; do {
			k -= NBLK;
Q			v2d* q = qq;
V			v2d* v = vw;
			rnd y01ct[NBLK][3];		// [0] = y0,  [1] =  y1,  [2] = cos(theta)^2
		#ifdef HI_LLIM
			int ny[NBLK/NWAY];		// exponent to extend double precision range.
			int lnz = llim;			// apriori, we go up to llim.
		#endif
			int imin = (k<k0) ? imin = k0-k-(NWAY-1) : 0;	// the last block may be smaller.
			for (int i=NBLK-NWAY; i>=imin; i-=NWAY) {		// iterate over blocks, start from the right-most (IMPORTANT!)
				rnd y0[NWAY], cost[NWAY];		// should fit in registers
				for (int j=0; j<NWAY; j++) {
					int idx = k+i+j;	if (idx < 0) idx=0;		// don't read below data.
					y0[j] = vall(mpos_scale);
					cost[j] = vread(st, idx);		// sin(theta)
				}
Q				l=m;
V				l=m-1;
				int ny0 = 0;
		#ifndef HI_LLIM
					do {		// sin(theta)^m
						if (l&1) for (int j=0; j<NWAY; ++j) y0[j] *= cost[j];
						for (int j=0; j<NWAY; ++j) cost[j] *= cost[j];
					} while(l >>= 1);
		#else  /* HI_LLIM */
				{	int nsint = 0;
					do {		// sin(theta)^m		(use rescaling to avoid underflow)
						if (l&1) {
							for (int j=NWAY-1; j>=0; --j) y0[j] *= cost[j];
							ny0 += nsint;
							if (vlo(y0[NWAY-1]) < (SHT_ACCURACY+1.0/SHT_SCALE_FACTOR)) {
								ny0--;
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
		#endif
				rnd y1[NWAY];
				al = alm;
				for (int j=0; j<NWAY; ++j) {
					int idx = k+i+j;	if (idx < 0) idx=0;		// don't read below data.
					cost[j] = vread(ct, idx);		// cos(theta)
					#ifndef SHTNS_ISHIOKA
					y0[j] *= vall(al[0]);
					y1[j]  = (vall(al[1])*y0[j]) * cost[j];
					#else
					cost[j] *= cost[j];		// cos(theta)^2
					y1[j] = (vall(al[1])*cost[j] + vall(al[0]))*y0[j];
					#endif
				}

				l=m;	al+=2;
		#ifdef HI_LLIM
				while ((ny0<0) && (l<lnz)) {		// ylm's are too small and are treated as zero
					#ifndef SHTNS_ISHIOKA
					for (int j=0; j<NWAY; ++j) {
						y0[j] = vall(al[1])*(cost[j]*y1[j]) + vall(al[0])*y0[j];
					}
					for (int j=0; j<NWAY; ++j) {
						y1[j] = vall(al[3])*(cost[j]*y0[j]) + vall(al[2])*y1[j];
					}
					l+=2;	al+=4;
					#else
					for (int j=0; j<NWAY; ++j) {
						rnd tmp = y1[j];
						y1[j] = (vall(al[1])*cost[j] + vall(al[0]))*y1[j] + y0[j];
						y0[j] = tmp;
					}
					l+=2;	al+=2;
					#endif
					if (fabs(vlo(y0[NWAY-1])) > SHT_ACCURACY*SHT_SCALE_FACTOR + 1.0) {		// rescale when value is significant
						++ny0;
						for (int j=0; j<NWAY; ++j) {
							y0[j] *= vall(1.0/SHT_SCALE_FACTOR);		y1[j] *= vall(1.0/SHT_SCALE_FACTOR);
						}
					}
				}
				ny[i/NWAY] = ny0;		// store extended exponents
QX				if ((l > llim) && (ny0<0)) break;	// nothing more to do in this block.
V				if ((l > llim+1) && (ny0<0)) break;	// nothing more to do in this block.
				lnz = l;				// record the minimum l for significant values, over this block, do not go above that afterwards
		#endif
				for (int j=0; j<NWAY; ++j) {	// store other stuff for recurrence.
					y01ct[i+j][0] = y0[j];
					y01ct[i+j][1] = y1[j];
					y01ct[i+j][2] = cost[j];
				}
			}
		#ifdef HI_LLIM
			if (ny[NBLK/NWAY-1] < 0) break;		// no ylm has a significant value in this block, the block is done.
			l = lnz;	// starting point in l for this block
		#endif

		#ifndef SHTNS_ISHIOKA
			al = alm + 2 + 2*(l-m);
			struct {
				rnd ct;
				rnd y[2];
Q				rnd rer, rei,  ror, roi;
V				rnd ter, tei,  tor, toi;
V				rnd per, pei,  por, poi;
			} x[NBLK];
Q			q+=(l-m);
V			v+=2*(l-m);
			imin = (k0-k > 0) ? k0-k : 0;	// 0 <= imin < NBLK
			for (int i=imin; i<NBLK; ++i) {
				rnd w = vread(wg, k+i);		x[i].ct = y01ct[i][2];
				x[i].y[0] = y01ct[i][0] * w;	x[i].y[1] = y01ct[i][1] * w;		// weight appears here (must be after the previous accuracy loop).
Q				x[i].rer = vread( rer, k+i);	x[i].rei = vread( rei, k+i);	x[i].ror = vread( ror, k+i);	x[i].roi = vread( roi, k+i);
V				x[i].ter = vread( ter, k+i);	x[i].tei = vread( tei, k+i);	x[i].tor = vread( tor, k+i);	x[i].toi = vread( toi, k+i);
V				x[i].per = vread( per, k+i);	x[i].pei = vread( pei, k+i);	x[i].por = vread( por, k+i);	x[i].poi = vread( poi, k+i);
			}
V			if (robert_form) {
V				for (int i=imin; i<NBLK; ++i) {
V					rnd st_1 = vread(shtns->st_1, k+i);
V					x[i].ter *= st_1;	x[i].tei *= st_1;	x[i].tor *= st_1;	x[i].toi *= st_1;
V					x[i].per *= st_1;	x[i].pei *= st_1;	x[i].por *= st_1;	x[i].poi *= st_1;
V				}
V			}
			while (l<llim) {	// compute even and odd parts
				unsigned j = imin;
			#ifdef HI_LLIM
				unsigned ii = imin/NWAY;
				while ((ny[ii] < 0) && (ii<NBLK/NWAY)) {		// these are zeros
					const unsigned imax = (ii+1)*NWAY;
					do {
						rnd y0 = x[j].y[0];		rnd y1 = x[j].y[1];
						y0 = vall(al[1])*(x[j].ct*y1) + vall(al[0])*y0;
						x[j].y[0] = y0;
						x[j].y[1] = vall(al[3])*(x[j].ct*y0) + vall(al[2])*y1;
					} while (++j < imax);
					if (fabs(vlo(x[ii*NWAY+NWAY-1].y[0])) > SHT_ACCURACY*SHT_SCALE_FACTOR + 1.0) {		// rescale when value is significant
						++ny[ii];
						for (int i=0; i<NWAY; ++i) {
							x[ii*NWAY+i].y[0] *= vall(1.0/SHT_SCALE_FACTOR);		x[ii*NWAY+i].y[1] *= vall(1.0/SHT_SCALE_FACTOR);
						}
					}
					++ii;
				}
			#endif
Q				rnd qq0 = vall(0.0);	// real
Q				rnd qq1 = vall(0.0);	// imag
Q				rnd qq2 = vall(0.0);	// real
Q				rnd qq3 = vall(0.0);	// real
V				rnd vv0 = vall(0.0);	rnd vv1 = vall(0.0);
V				rnd ww0 = vall(0.0);	rnd ww1 = vall(0.0);
V				rnd vv2 = vall(0.0);	rnd vv3 = vall(0.0);
V				rnd ww2 = vall(0.0);	rnd ww3 = vall(0.0);
				while (j<NBLK) {
					register rnd y0 = x[j].y[0];
					register rnd y1 = x[j].y[1];
					{
Q						qq0 += y0 * x[j].rer;		qq1 += y0 * x[j].rei;	// real even, imag even
V						vv0 += y0 * x[j].ter;		vv1 += y0 * x[j].tei;	// real even, imag even
V						ww0 += y0 * x[j].per;		ww1 += y0 * x[j].pei;	// real even, imag even
Q						qq2 += y1 * x[j].ror;		qq3 += y1 * x[j].roi;	// real even, imag even
V						vv2 += y1 * x[j].tor;		vv3 += y1 * x[j].toi;	// real odd, imag odd
V						ww2 += y1 * x[j].por;		ww3 += y1 * x[j].poi;	// real odd, imag odd
					}
					y0 = vall(al[1])*(x[j].ct*y1) + vall(al[0])*y0;
					x[j].y[0] = y0;
					x[j].y[1] = vall(al[3])*(x[j].ct*y0) + vall(al[2])*y1;
					++j;
				}
Q				q[0] += v2d_reduce(qq0, qq1);
Q				q[1] += v2d_reduce(qq2, qq3);
V				v[0] += v2d_reduce(vv0, vv1);
V				v[1] += v2d_reduce(ww0, ww1);
V				v[2] += v2d_reduce(vv2, vv3);
V				v[3] += v2d_reduce(ww2, ww3);
Q				q+=2;
V				v+=4;
				l+=2;	al+=4;
			}
			{
V				rnd vv0 = vall(0.0);
V				rnd vv1 = vall(0.0);
V				rnd ww0 = vall(0.0);
V				rnd ww1 = vall(0.0);
				for (unsigned j=imin; j<NBLK; ++j) {
					#ifdef HI_LLIM
					if (ny[j/NWAY] == 0)
					#endif
					{
						register rnd y0 = x[j].y[0];
V						vv0 += y0 * x[j].ter;		vv1 += y0 * x[j].tei;	// real even, imag even
V						ww0 += y0 * x[j].per;		ww1 += y0 * x[j].pei;	// real even, imag even
					}
				}
V				v[0] += v2d_reduce(vv0, vv1);
V				v[1] += v2d_reduce(ww0, ww1);
			}
			if (l==llim) {
Q				rnd qq0 = vall(0.0);	// real
Q				rnd qq1 = vall(0.0);	// imag
V				rnd vv0 = vall(0.0);
V				rnd vv1 = vall(0.0);
V				rnd ww0 = vall(0.0);
V				rnd ww1 = vall(0.0);
				for (unsigned j=imin; j<NBLK; ++j) {
					#ifdef HI_LLIM
					if (ny[j/NWAY] == 0)
					#endif
					{
						register rnd y0 = x[j].y[0];
Q						qq0 += y0 * x[j].rer;		qq1 += y0 * x[j].rei;	// real even, imag even
					}
				}
				for (unsigned j=imin; j<NBLK; ++j) {
					#ifdef HI_LLIM
					if (ny[j/NWAY] == 0)
					#endif
					{
						register rnd y1 = x[j].y[1];
V						vv0 += y1 * x[j].tor;		vv1 += y1 * x[j].toi;	// real odd, imag odd
V						ww0 += y1 * x[j].por;		ww1 += y1 * x[j].poi;	// real odd, imag odd
					}
				}
Q				q[0] += v2d_reduce(qq0, qq1);
V				v[2] += v2d_reduce(vv0, vv1);
V				v[3] += v2d_reduce(ww0, ww1);
			}
		#else    /* SHTNS_ISHIOKA */
			al = alm + 2 + (l-m);
			struct {
				rnd y[2];
				rnd ct2;
Q				rnd rer, rei,  ror, roi;
V				rnd ter, tei,  tor, toi;
V				rnd per, pei,  por, poi;
			} x[NBLK];
Q			q+=(l-m);
V			v+=2*(l-m);
			imin = (k0-k > 0) ? k0-k : 0;	// 0 <= imin < NBLK
			for (int i=imin; i<NBLK; ++i) {
Q				x[i].rer = vread( rer, k+i);	x[i].rei = vread( rei, k+i);	x[i].ror = vread( ror, k+i);	x[i].roi = vread( roi, k+i);
V				x[i].ter = vread( ter, k+i);	x[i].tei = vread( tei, k+i);	x[i].tor = vread( tor, k+i);	x[i].toi = vread( toi, k+i);
V				x[i].per = vread( per, k+i);	x[i].pei = vread( pei, k+i);	x[i].por = vread( por, k+i);	x[i].poi = vread( poi, k+i);
				x[i].y[0] = y01ct[i][0];		x[i].y[1] = y01ct[i][1];		x[i].ct2 = y01ct[i][2];
			}
V			if (robert_form) {
V				for (int i=imin; i<NBLK; ++i) {
V					rnd st_1 = vread(shtns->st_1, k+i);
V					x[i].ter *= st_1;	x[i].tei *= st_1;	x[i].tor *= st_1;	x[i].toi *= st_1;
V					x[i].per *= st_1;	x[i].pei *= st_1;	x[i].por *= st_1;	x[i].poi *= st_1;
V				}
V			}
QX			while (l<=llim) {	// compute even and odd parts
V			while (l<=llim+1) {	// compute even and odd parts
				unsigned i = imin;
			#ifdef HI_LLIM
				unsigned ii = imin/NWAY;
				while ((ny[ii] < 0) && (ii<NBLK/NWAY)) {		// these are zeros
					const unsigned imax = (ii+1)*NWAY;
					do {
						if (LSPAN==2) {
							rnd tmp = x[i].y[1];
							x[i].y[1] = (vall(al[1])*x[i].ct2 + vall(al[0]))*tmp + x[i].y[0];
							x[i].y[0] = tmp;
						} else {	// LSPAN == 4
							x[i].y[0] = (vall(al[1])*x[i].ct2 + vall(al[0]))*x[i].y[1] + x[i].y[0];
							x[i].y[1] = (vall(al[3])*x[i].ct2 + vall(al[2]))*x[i].y[0] + x[i].y[1];						
						}
					} while (++i < imax);
					if (fabs(vlo(x[ii*NWAY+NWAY-1].y[0])) > SHT_ACCURACY*SHT_SCALE_FACTOR + 1.0) {		// rescale when value is significant
						++ny[ii];
						for (int j=0; j<NWAY; ++j) {
							x[ii*NWAY+j].y[0] *= vall(1.0/SHT_SCALE_FACTOR);		x[ii*NWAY+j].y[1] *= vall(1.0/SHT_SCALE_FACTOR);
						}
					}
					++ii;
				}
			#endif
Q				rnd ql[2*LSPAN];
V				rnd vl[2*LSPAN];		rnd wl[2*LSPAN];
				for (int ll=0; ll<2*LSPAN; ll++) {
Q					ql[ll] = vall(0.0);
V					vl[ll] = vall(0.0);		wl[ll] = vall(0.0);
				}
				while (i<NBLK) {
					for (int ll=0; ll<LSPAN/2; ll++) {
						register rnd y = x[i].y[ll];
Q						ql[4*ll+0] += y * x[i].rer;		ql[4*ll+1] += y * x[i].rei;
Q						ql[4*ll+2] += y * x[i].ror;		ql[4*ll+3] += y * x[i].roi;
V						vl[4*ll+0] += y * x[i].ter;		vl[4*ll+1] += y * x[i].tei;
V						vl[4*ll+2] += y * x[i].tor;		vl[4*ll+3] += y * x[i].toi;
V						wl[4*ll+0] += y * x[i].per;		wl[4*ll+1] += y * x[i].pei;
V						wl[4*ll+2] += y * x[i].por;		wl[4*ll+3] += y * x[i].poi;
					}
					if (LSPAN==2) {
						register rnd tmp = x[i].y[1];
						x[i].y[1] = (vall(al[1])*x[i].ct2 + vall(al[0]))*tmp + x[i].y[0];
						x[i].y[0] = tmp;
					} else {	// LSPAN == 4
						x[i].y[0] = (vall(al[1])*x[i].ct2 + vall(al[0]))*x[i].y[1] + x[i].y[0];
						x[i].y[1] = (vall(al[3])*x[i].ct2 + vall(al[2]))*x[i].y[0] + x[i].y[1];						
					}
					++i;
				}
Q				q[0] += v2d_reduce(ql[0], ql[1]);
V				v[0] += v2d_reduce(vl[0], vl[1]);
V				v[1] += v2d_reduce(wl[0], wl[1]);
				for (int ll=1; ll<LSPAN; ll++) {
					if (l+ll<=llim) {
Q						q[ll]     += v2d_reduce(ql[2*ll], ql[2*ll+1]);
					}
					if (l+ll<=llim+1) {
V						v[2*ll]   += v2d_reduce(vl[2*ll], vl[2*ll+1]);
V						v[2*ll+1] += v2d_reduce(wl[2*ll], wl[2*ll+1]);
					}
				}
Q				q+=LSPAN;
V				v+=2*LSPAN;
				l+=LSPAN;	al+=LSPAN;
			}
	/*		{
V				rnd vv0 = y0[0] * terk[0];
V				rnd vv1 = y0[0] * teik[0];
V				rnd ww0 = y0[0] * perk[0];
V				rnd ww1 = y0[0] * peik[0];
V				for (int j=1; j<NW; ++j) {	vv0 += y0[j] * terk[j];		vv1 += y0[j] * teik[j];	}	// real even, imag even
V				for (int j=1; j<NW; ++j) {	ww0 += y0[j] * perk[j];		ww1 += y0[j] * peik[j];	}	// real even, imag even
V				v[0] += v2d_reduce(vv0, vv1);
V				v[1] += v2d_reduce(ww0, ww1);
			}
			if (l==llim) {
Q		//		rnd qq0 = y0[0] * rerk[0];
Q		//		rnd qq1 = y0[0] * reik[0];
V				rnd vv0 = y0[0] * tork[0];
V				rnd vv1 = y0[0] * toik[0];
V				rnd ww0 = y0[0] * pork[0];
V				rnd ww1 = y0[0] * poik[0];
Q		//		for (int j=1; j<NW; ++j) {	qq0 += y0[j] * rerk[j];		qq1 += y0[j] * reik[j];	}	// real even, imag even
V				for (int j=1; j<NW; ++j) {	vv0 += y0[j] * tork[j];		vv1 += y0[j] * toik[j]; }	// real odd, imag odd
V				for (int j=1; j<NW; ++j) {	ww0 += y0[j] * pork[j];		ww1 += y0[j] * poik[j]; }	// real odd, imag odd
Q		//		q[0] += v2d_reduce(qq0, qq1);
V				v[2] += v2d_reduce(vv0, vv1);
V				v[3] += v2d_reduce(ww0, ww1);
			}	*/
		#endif
		} while (k > k0);
		
		l = LiM(shtns, m, im);
Q		v2d *Ql = (v2d*) &Qlm[l];
V		v2d *Sl = (v2d*) &Slm[l];
V		v2d *Tl = (v2d*) &Tlm[l];

	#ifndef SHTNS_ISHIOKA
Q		for (l=0; l<=llim-m; ++l)	Ql[l] = qq[l];
	#else
		// post-processing for recurrence relation of Ishioka
		const double* restrict xlm = shtns->x2lm + 3*im*(2*(LMAX+4) -m+MRES)/4;
Q		ishioka_to_SH(xlm, qq, llim-m, Ql);
V		ishioka_to_SH2(xlm, vw, llim-m+1, vw);
	#endif

V		SH_2scal_to_vect(shtns->mx_van + 2*LM(shtns,m,m), l_2, llim, m, vw, Sl, Tl);

		#ifdef SHT_VAR_LTR
			for (l=llim+1-m; l<=LMAX-m; ++l) {
Q				Ql[l] = vdup(0.0);
V				Sl[l] = vdup(0.0);		Tl[l] = vdup(0.0);
			}
		#endif
	}
  #endif
  }

	#undef BASE
	#undef LSPAN
