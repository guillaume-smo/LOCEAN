!WRF:MEDIATION_LAYER:PHYSICS
!

MODULE module_diagnostics
CONTAINS
   SUBROUTINE diagnostic_output_calc(                                 &
                      ids,ide, jds,jde, kds,kde,                      &
                      ims,ime, jms,jme, kms,kme,                      &
                      ips,ipe, jps,jpe, kps,kpe,                      & ! patch  dims
                      i_start,i_end,j_start,j_end,kts,kte,num_tiles   &
                     ,dpsdt,dmudt                                     &
                     ,p8w,pk1m,mu_2,mu_2m                             &
                     ,u,v                                             &
                     ,raincv,rainncv,rainc,rainnc                     &
                     ,i_rainc,i_rainnc                                &
                     ,hfx,sfcevp,lh                                   &
                     ,ACSWUPT,ACSWUPTC,ACSWDNT,ACSWDNTC               & ! Optional
                     ,ACSWUPB,ACSWUPBC,ACSWDNB,ACSWDNBC               & ! Optional
                     ,ACLWUPT,ACLWUPTC,ACLWDNT,ACLWDNTC               & ! Optional
                     ,ACLWUPB,ACLWUPBC,ACLWDNB,ACLWDNBC               & ! Optional
!                     ,I_ACSWUPT,I_ACSWUPTC,I_ACSWDNT,I_ACSWDNTC       & ! Optional
!                     ,I_ACSWUPB,I_ACSWUPBC,I_ACSWDNB,I_ACSWDNBC       & ! Optional
!                     ,I_ACLWUPT,I_ACLWUPTC,I_ACLWDNT,I_ACLWDNTC       & ! Optional
!                     ,I_ACLWUPB,I_ACLWUPBC,I_ACLWDNB,I_ACLWDNBC       & ! Optional
                     ,SWUPT,SWUPTC,SWDNT,SWDNTC                       & ! Optional
                     ,SWUPB,SWUPBC,SWDNB,SWDNBC                       & ! Optional
                     ,LWUPT,LWUPTC,LWDNT,LWDNTC                       & ! Optional
                     ,LWUPB,LWUPBC,LWDNB,LWDNBC                       & ! Optional
                     ,dt,xtime,sbw,t2                                 &
                     ,diag_print                                      &
                     ,bucket_mm, bucket_J                             &
                     ,prec_acc_c, prec_acc_nc, snow_acc_nc            &
                     ,snowncv, prec_acc_dt, curr_secs                 &
                                                                      )
!----------------------------------------------------------------------

  USE module_dm, ONLY: wrf_dm_sum_real, wrf_dm_maxval

   IMPLICIT NONE
!======================================================================
! Definitions
!-----------
!-- DIAG_PRINT    print control: 0 - no diagnostics; 1 - dmudt only; 2 - all
!-- DT            time step (second)
!-- XTIME         forecast time
!-- SBW           specified boundary width - used later
!
!-- P8W           3D pressure array at full eta levels
!-- MU            dry column hydrostatic pressure
!-- RAINC         cumulus scheme precipitation since hour 0
!-- RAINCV        cumulus scheme precipitation in one time step (mm)
!-- RAINNC        explicit scheme precipitation since hour 0
!-- RAINNCV       explicit scheme precipitation in one time step (mm)
!-- SNOWNCV       explicit scheme snow in one time step (mm)
!-- HFX           surface sensible heat flux
!-- LH            surface latent heat flux
!-- SFCEVP        total surface evaporation
!-- U             u component of wind - to be used later to compute k.e.
!-- V             v component of wind - to be used later to compute k.e.
!-- PREC_ACC_C    accumulated convective precip over accumulation time prec_acc_dt
!-- PREC_ACC_NC   accumulated explicit precip over accumulation time prec_acc_dt
!-- SNOW_ACC_NC   accumulated explicit snow precip over accumulation time prec_acc_dt
!-- PREC_ACC_DT   precip accumulation time, default is 60 min
!-- CURR_SECS     model time in seconds
!
!-- ids           start index for i in domain
!-- ide           end index for i in domain
!-- jds           start index for j in domain
!-- jde           end index for j in domain
!-- kds           start index for k in domain
!-- kde           end index for k in domain
!-- ims           start index for i in memory
!-- ime           end index for i in memory
!-- jms           start index for j in memory
!-- jme           end index for j in memory
!-- ips           start index for i in patch
!-- ipe           end index for i in patch
!-- jps           start index for j in patch
!-- jpe           end index for j in patch
!-- kms           start index for k in memory
!-- kme           end index for k in memory
!-- i_start       start indices for i in tile
!-- i_end         end indices for i in tile
!-- j_start       start indices for j in tile
!-- j_end         end indices for j in tile
!-- kts           start index for k in tile
!-- kte           end index for k in tile
!-- num_tiles     number of tiles
!
!======================================================================

   INTEGER,      INTENT(IN   )    ::                             &
                                      ids,ide, jds,jde, kds,kde, &
                                      ims,ime, jms,jme, kms,kme, &
                                      ips,ipe, jps,jpe, kps,kpe, &
                                                        kts,kte, &
                                                      num_tiles

   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                  &
     &           i_start,i_end,j_start,j_end

   INTEGER,      INTENT(IN   )    ::   diag_print
   REAL,      INTENT(IN   )    ::   bucket_mm, bucket_J

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         INTENT(IN ) ::                                       u  &
                                                    ,         v  &
                                                    ,       p8w

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN) ::           &
                                                           MU_2  &
                                                    ,   RAINNCV  &
                                                    ,    RAINCV  &
                                                    ,   SNOWNCV  &
                                                    ,       HFX  &
                                                    ,        LH  &
                                                    ,    SFCEVP  &  
                                                    ,        T2     

   REAL, DIMENSION( ims:ime , jms:jme ),                         &
          INTENT(INOUT) ::                                DPSDT  &
                                                    ,     DMUDT  &
                                                    ,    RAINNC  &
                                                    ,     RAINC  &
                                                    ,     MU_2M  &
                                                    ,      PK1M
 
   REAL,  INTENT(IN   ) :: DT, XTIME
   INTEGER,  INTENT(IN   ) :: SBW
   INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) ::     &
                                                       I_RAINC,  &
                                                       I_RAINNC
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT) ::&
                      ACSWUPT,ACSWUPTC,ACSWDNT,ACSWDNTC,          &
                      ACSWUPB,ACSWUPBC,ACSWDNB,ACSWDNBC,          &
                      ACLWUPT,ACLWUPTC,ACLWDNT,ACLWDNTC,          &
                      ACLWUPB,ACLWUPBC,ACLWDNB,ACLWDNBC
!   INTEGER, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT) ::&
!                      I_ACSWUPT,I_ACSWUPTC,I_ACSWDNT,I_ACSWDNTC,  &
!                      I_ACSWUPB,I_ACSWUPBC,I_ACSWDNB,I_ACSWDNBC,  &
!                      I_ACLWUPT,I_ACLWUPTC,I_ACLWDNT,I_ACLWDNTC,  &
!                      I_ACLWUPB,I_ACLWUPBC,I_ACLWDNB,I_ACLWDNBC
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT) ::&
                      SWUPT,SWUPTC,SWDNT,SWDNTC,  &
                      SWUPB,SWUPBC,SWDNB,SWDNBC,  &
                      LWUPT,LWUPTC,LWDNT,LWDNTC,  &
                      LWUPB,LWUPBC,LWDNB,LWDNBC

   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT) ::&
                      PREC_ACC_C, PREC_ACC_NC, SNOW_ACC_NC

   REAL, OPTIONAL, INTENT(IN)::  PREC_ACC_DT, CURR_SECS

   INTEGER :: i,j,k,its,ite,jts,jte,ij
   INTEGER :: idp,jdp,irc,jrc,irnc,jrnc,isnh,jsnh
   INTEGER :: prfreq

   REAL              :: no_points
   REAL              :: dpsdt_sum, dmudt_sum, dardt_sum, drcdt_sum, drndt_sum
   REAL              :: hfx_sum, lh_sum, sfcevp_sum, rainc_sum, rainnc_sum, raint_sum
   REAL              :: dmumax, raincmax, rainncmax, snowhmax
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   CHARACTER*256     :: outstring
   CHARACTER*6       :: grid_str

!-----------------------------------------------------------------
! Handle accumulations with buckets to prevent round-off truncation in long runs
! This is done every 360 minutes assuming time step fits exactly into 360 minutes
   IF(bucket_mm .gt. 0. .AND. MOD(NINT(XTIME),360) .EQ. 0)THEN
! SET START AND END POINTS FOR TILES
!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )

   DO ij = 1 , num_tiles

      IF (xtime .eq. 0.0)THEN
        DO j=j_start(ij),j_end(ij)
        DO i=i_start(ij),i_end(ij)
          i_rainnc(i,j) = 0
          i_rainc(i,j) = 0
        ENDDO      
        ENDDO
      ENDIF
      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)
        IF(rainnc(i,j) .gt. bucket_mm)THEN
          rainnc(i,j) = rainnc(i,j) - bucket_mm
          i_rainnc(i,j) =  i_rainnc(i,j) + 1
        ENDIF
        IF(rainc(i,j) .gt. bucket_mm)THEN
          rainc(i,j) = rainc(i,j) - bucket_mm
          i_rainc(i,j) =  i_rainc(i,j) + 1
        ENDIF
      ENDDO      
      ENDDO

!      IF (xtime .eq. 0.0 .and. bucket_J .gt. 0.0 .and. PRESENT(ACSWUPT))THEN
!        DO j=j_start(ij),j_end(ij)
!        DO i=i_start(ij),i_end(ij)
!          i_acswupt(i,j) = 0
!          i_acswuptc(i,j) = 0
!          i_acswdnt(i,j) = 0
!          i_acswdntc(i,j) = 0
!          i_acswupb(i,j) = 0
!          i_acswupbc(i,j) = 0
!          i_acswdnb(i,j) = 0
!          i_acswdnbc(i,j) = 0
!        ENDDO      
!        ENDDO
!      ENDIF
!      IF (xtime .eq. 0.0  .and. bucket_J .gt. 0.0 .and. PRESENT(ACLWUPT))THEN
!        DO j=j_start(ij),j_end(ij)
!        DO i=i_start(ij),i_end(ij)
!          i_aclwupt(i,j) = 0
!          i_aclwuptc(i,j) = 0
!          i_aclwdnt(i,j) = 0
!          i_aclwdntc(i,j) = 0
!          i_aclwupb(i,j) = 0
!          i_aclwupbc(i,j) = 0
!          i_aclwdnb(i,j) = 0
!          i_aclwdnbc(i,j) = 0
!        ENDDO      
!        ENDDO
!      ENDIF
!      IF (PRESENT(ACSWUPT) .and. bucket_J .gt. 0.0)THEN
!      DO j=j_start(ij),j_end(ij)
!      DO i=i_start(ij),i_end(ij)
!        IF(acswupt(i,j) .gt. bucket_J)THEN
!          acswupt(i,j) = acswupt(i,j) - bucket_J
!          i_acswupt(i,j) =  i_acswupt(i,j) + 1
!        ENDIF
!        IF(acswuptc(i,j) .gt. bucket_J)THEN
!          acswuptc(i,j) = acswuptc(i,j) - bucket_J
!          i_acswuptc(i,j) =  i_acswuptc(i,j) + 1
!        ENDIF
!        IF(acswdnt(i,j) .gt. bucket_J)THEN
!          acswdnt(i,j) = acswdnt(i,j) - bucket_J
!          i_acswdnt(i,j) =  i_acswdnt(i,j) + 1
!        ENDIF
!        IF(acswdntc(i,j) .gt. bucket_J)THEN
!          acswdntc(i,j) = acswdntc(i,j) - bucket_J
!          i_acswdntc(i,j) =  i_acswdntc(i,j) + 1
!        ENDIF
!        IF(acswupb(i,j) .gt. bucket_J)THEN
!          acswupb(i,j) = acswupb(i,j) - bucket_J
!          i_acswupb(i,j) =  i_acswupb(i,j) + 1
!        ENDIF
!        IF(acswupbc(i,j) .gt. bucket_J)THEN
!          acswupbc(i,j) = acswupbc(i,j) - bucket_J
!          i_acswupbc(i,j) =  i_acswupbc(i,j) + 1
!        ENDIF
!        IF(acswdnb(i,j) .gt. bucket_J)THEN
!          acswdnb(i,j) = acswdnb(i,j) - bucket_J
!          i_acswdnb(i,j) =  i_acswdnb(i,j) + 1
!        ENDIF
!        IF(acswdnbc(i,j) .gt. bucket_J)THEN
!          acswdnbc(i,j) = acswdnbc(i,j) - bucket_J
!          i_acswdnbc(i,j) =  i_acswdnbc(i,j) + 1
!        ENDIF
!      ENDDO      
!      ENDDO
!      ENDIF
!      IF (PRESENT(ACLWUPT) .and. bucket_J .gt. 0.0)THEN
!      DO j=j_start(ij),j_end(ij)
!      DO i=i_start(ij),i_end(ij)
!        IF(aclwupt(i,j) .gt. bucket_J)THEN
!          aclwupt(i,j) = aclwupt(i,j) - bucket_J
!          i_aclwupt(i,j) =  i_aclwupt(i,j) + 1
!        ENDIF
!        IF(aclwuptc(i,j) .gt. bucket_J)THEN
!          aclwuptc(i,j) = aclwuptc(i,j) - bucket_J
!          i_aclwuptc(i,j) =  i_aclwuptc(i,j) + 1
!        ENDIF
!        IF(aclwdnt(i,j) .gt. bucket_J)THEN
!          aclwdnt(i,j) = aclwdnt(i,j) - bucket_J
!          i_aclwdnt(i,j) =  i_aclwdnt(i,j) + 1
!        ENDIF
!        IF(aclwdntc(i,j) .gt. bucket_J)THEN
!          aclwdntc(i,j) = aclwdntc(i,j) - bucket_J
!          i_aclwdntc(i,j) =  i_aclwdntc(i,j) + 1
!        ENDIF
!        IF(aclwupb(i,j) .gt. bucket_J)THEN
!          aclwupb(i,j) = aclwupb(i,j) - bucket_J
!          i_aclwupb(i,j) =  i_aclwupb(i,j) + 1
!        ENDIF
!        IF(aclwupbc(i,j) .gt. bucket_J)THEN
!          aclwupbc(i,j) = aclwupbc(i,j) - bucket_J
!          i_aclwupbc(i,j) =  i_aclwupbc(i,j) + 1
!        ENDIF
!        IF(aclwdnb(i,j) .gt. bucket_J)THEN
!          aclwdnb(i,j) = aclwdnb(i,j) - bucket_J
!          i_aclwdnb(i,j) =  i_aclwdnb(i,j) + 1
!        ENDIF
!        IF(aclwdnbc(i,j) .gt. bucket_J)THEN
!          aclwdnbc(i,j) = aclwdnbc(i,j) - bucket_J
!          i_aclwdnbc(i,j) =  i_aclwdnbc(i,j) + 1
!        ENDIF
!      ENDDO      
!      ENDDO
!      ENDIF
!
   ENDDO
!  !$OMP END PARALLEL DO
   ENDIF

! Compute precipitation accumulation in a given time window: prec_acc_dt
   IF (prec_acc_dt .gt. 0.) THEN

!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )

   DO ij = 1 , num_tiles

      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)
         IF (mod(curr_secs, 60.* prec_acc_dt) == 0.) THEN
            prec_acc_c(i,j)  = 0.
            prec_acc_nc(i,j) = 0.
            snow_acc_nc(i,j)  = 0.
         ENDIF
         prec_acc_c(i,j)  = prec_acc_c(i,j)  +  RAINCV(i,j)
         prec_acc_nc(i,j) = prec_acc_nc(i,j) + RAINNCV(i,j)
         prec_acc_c(i,j)  = MAX (prec_acc_c(i,j), 0.0)
         prec_acc_nc(i,j) = MAX (prec_acc_nc(i,j), 0.0)
         snow_acc_nc(i,j)   = snow_acc_nc(i,j) + SNOWNCV(I,J)
! add convective precip to snow bucket if t2 < 273.15
         IF ( t2(i,j) .lt. 273.15 ) THEN
         snow_acc_nc(i,j)   = snow_acc_nc(i,j) +  RAINCV(i,j)
         snow_acc_nc(i,j)   = MAX (snow_acc_nc(i,j), 0.0)
         ENDIF
      ENDDO     
      ENDDO     

      IF ( PRESENT(ACSWUPT) )THEN
         DO j=j_start(ij),j_end(ij)
         DO i=i_start(ij),i_end(ij)
            IF (mod(curr_secs, 60.* prec_acc_dt) == 0.) THEN
               acswupt(i,j)  = 0.
               acswuptc(i,j) = 0.
               acswdnt(i,j)  = 0.
               acswdntc(i,j) = 0.
               acswupb(i,j)  = 0.
               acswupbc(i,j) = 0.
               acswdnb(i,j)  = 0.
               acswdnbc(i,j) = 0.
            ENDIF
            acswupt(i,j)  = acswupt(i,j) + swupt(i,j)
            acswuptc(i,j) = acswuptc(i,j) + swuptc(i,j)
            acswdnt(i,j)  = acswdnt(i,j) + swdnt(i,j)
            acswdntc(i,j) = acswdntc(i,j) + swdntc(i,j)
            acswupb(i,j)  = acswupb(i,j) + swupb(i,j)
            acswupbc(i,j) = acswupbc(i,j) + swupbc(i,j)
            acswdnb(i,j)  = acswdnb(i,j) + swdnb(i,j)
            acswdnbc(i,j) = acswdnbc(i,j) + swdnbc(i,j)
         ENDDO
         ENDDO
      ENDIF

      IF ( PRESENT(ACLWUPT) )THEN
         DO j=j_start(ij),j_end(ij)
         DO i=i_start(ij),i_end(ij)
            IF (mod(curr_secs, 60.* prec_acc_dt) == 0.) THEN
               aclwupt(i,j)  = 0.
               aclwuptc(i,j) = 0.
               aclwdnt(i,j)  = 0.
               aclwdntc(i,j) = 0.
               aclwupb(i,j)  = 0.
               aclwupbc(i,j) = 0.
               aclwdnb(i,j)  = 0.
               aclwdnbc(i,j) = 0.
            ENDIF
            aclwupt(i,j)  = aclwupt(i,j) + lwupt(i,j)
            aclwuptc(i,j) = aclwuptc(i,j) + lwuptc(i,j)
            aclwdnt(i,j)  = aclwdnt(i,j) + lwdnt(i,j)
            aclwdntc(i,j) = aclwdntc(i,j) + lwdntc(i,j)
            aclwupb(i,j)  = aclwupb(i,j) + lwupb(i,j)
            aclwupbc(i,j) = aclwupbc(i,j) + lwupbc(i,j)
            aclwdnb(i,j)  = aclwdnb(i,j) + lwdnb(i,j)
            aclwdnbc(i,j) = aclwdnbc(i,j) + lwdnbc(i,j)
         ENDDO
         ENDDO
      ENDIF

   ENDDO     

!  !$OMP END PARALLEL DO
   ENDIF

   if (diag_print .eq. 0 ) return

   IF ( xtime .ne. 0. ) THEN

    if(diag_print.eq.1) then
       prfreq = dt
!      prfreq = max(2,int(dt/60.))   ! in min
    else
       prfreq=10                   ! in min
    endif
   
    IF (MOD(nint(dt),prfreq) == 0) THEN

! COMPUTE THE NUMBER OF MASS GRID POINTS
   no_points = float((ide-ids)*(jde-jds))

! SET START AND END POINTS FOR TILES
!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )

   dmumax = 0.
   DO ij = 1 , num_tiles

!     print *, i_start(ij),i_end(ij),j_start(ij),j_end(ij)
      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)
         dpsdt(i,j)=(p8w(i,kms,j)-pk1m(i,j))/dt
         dmudt(i,j)=(mu_2(i,j)-mu_2m(i,j))/dt
         if(abs(dmudt(i,j)*dt).gt.dmumax)then
           dmumax=abs(dmudt(i,j)*dt)
           idp=i
           jdp=j
         endif
      ENDDO      
      ENDDO

   ENDDO
!  !$OMP END PARALLEL DO

! convert DMUMAX from (PA) to (bars) per time step
   dmumax = dmumax*1.e-5
! compute global MAX
   CALL wrf_dm_maxval ( dmumax,  idp, jdp )

!  print *, 'p8w(30,1,30),pk1m(30,30) : ', p8w(30,1,30),pk1m(30,30)
!  print *, 'mu_2(30,30),mu_2m(30,30) : ', mu_2(30,30),mu_2m(30,30)
   dpsdt_sum = 0.
   dmudt_sum = 0.

   DO j = jps, min(jpe,jde-1)
     DO i = ips, min(ipe,ide-1)
       dpsdt_sum = dpsdt_sum + abs(dpsdt(i,j))
       dmudt_sum = dmudt_sum + abs(dmudt(i,j))
     ENDDO
   ENDDO

! compute global sum
   dpsdt_sum = wrf_dm_sum_real ( dpsdt_sum )
   dmudt_sum = wrf_dm_sum_real ( dmudt_sum )

!  print *, 'dpsdt, dmudt : ', dpsdt_sum, dmudt_sum

   IF ( diag_print .eq. 2 ) THEN
   dardt_sum = 0.
   drcdt_sum = 0.
   drndt_sum = 0.
   rainc_sum = 0.
   raint_sum = 0.
   rainnc_sum = 0.
   sfcevp_sum = 0.
   hfx_sum = 0.
   lh_sum = 0.
   raincmax = 0.
   rainncmax = 0.

   DO j = jps, min(jpe,jde-1)
     DO i = ips, min(ipe,ide-1)
       drcdt_sum = drcdt_sum + abs(raincv(i,j))
       drndt_sum = drndt_sum + abs(rainncv(i,j))
       dardt_sum = dardt_sum + abs(raincv(i,j)) + abs(rainncv(i,j))
       rainc_sum = rainc_sum + abs(rainc(i,j))
! MAX for accumulated conv precip
       IF(rainc(i,j).gt.raincmax)then
          raincmax=rainc(i,j)
          irc=i
          jrc=j
       ENDIF
       rainnc_sum = rainnc_sum + abs(rainnc(i,j))
! MAX for accumulated resolved precip
       IF(rainnc(i,j).gt.rainncmax)then
          rainncmax=rainnc(i,j)
          irnc=i
          jrnc=j
       ENDIF
       raint_sum = raint_sum + abs(rainc(i,j)) + abs(rainnc(i,j))
       sfcevp_sum = sfcevp_sum + abs(sfcevp(i,j))
       hfx_sum = hfx_sum + abs(hfx(i,j))
       lh_sum = lh_sum + abs(lh(i,j))
     ENDDO
   ENDDO

! compute global MAX
   CALL wrf_dm_maxval ( raincmax, irc, jrc )
   CALL wrf_dm_maxval ( rainncmax, irnc, jrnc )

! compute global sum
   drcdt_sum = wrf_dm_sum_real ( drcdt_sum )
   drndt_sum = wrf_dm_sum_real ( drndt_sum )
   dardt_sum = wrf_dm_sum_real ( dardt_sum )
   rainc_sum = wrf_dm_sum_real ( rainc_sum )
   rainnc_sum = wrf_dm_sum_real ( rainnc_sum )
   raint_sum = wrf_dm_sum_real ( raint_sum )
   sfcevp_sum = wrf_dm_sum_real ( sfcevp_sum )
   hfx_sum = wrf_dm_sum_real ( hfx_sum )
   lh_sum = wrf_dm_sum_real ( lh_sum )

   ENDIF

! print out the average values

   CALL get_current_grid_name( grid_str )

#ifdef DM_PARALLEL
   IF ( wrf_dm_on_monitor() ) THEN
#endif
     WRITE(outstring,*) grid_str,'Domain average of dpsdt, dmudt (mb/3h): ', xtime, &
           dpsdt_sum/no_points*108., &
           dmudt_sum/no_points*108.
     CALL wrf_message ( TRIM(outstring) )

     WRITE(outstring,*) grid_str,'Max mu change time step: ', idp,jdp,dmumax
     CALL wrf_message ( TRIM(outstring) )

     IF ( diag_print .eq. 2) THEN
     WRITE(outstring,*) grid_str,'Domain average of dardt, drcdt, drndt (mm/sec): ', xtime, &
           dardt_sum/dt/no_points, &
           drcdt_sum/dt/no_points, &
           drndt_sum/dt/no_points
     CALL wrf_message ( TRIM(outstring) )
     WRITE(outstring,*) grid_str,'Domain average of rt_sum, rc_sum, rnc_sum (mm): ', xtime, &
           raint_sum/no_points, &
           rainc_sum/no_points, &
           rainnc_sum/no_points
     CALL wrf_message ( TRIM(outstring) )
     WRITE(outstring,*) grid_str,'Max Accum Resolved Precip,   I,J  (mm): '               ,&
           rainncmax,irnc,jrnc
     CALL wrf_message ( TRIM(outstring) )
     WRITE(outstring,*) grid_str,'Max Accum Convective Precip,   I,J  (mm): '             ,&
           raincmax,irc,jrc
     CALL wrf_message ( TRIM(outstring) )
     WRITE(outstring,*) grid_str,'Domain average of sfcevp, hfx, lh: ', xtime, &
           sfcevp_sum/no_points, &
           hfx_sum/no_points, &
           lh_sum/no_points
     CALL wrf_message ( TRIM(outstring) )
     ENDIF
#ifdef DM_PARALLEL
   ENDIF
#endif

    ENDIF        ! print frequency
   ENDIF

! save values at this time step
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij,i,j )
   DO ij = 1 , num_tiles

      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)
         pk1m(i,j)=p8w(i,kms,j)
         mu_2m(i,j)=mu_2(i,j)
      ENDDO
      ENDDO

      IF ( xtime .lt. 0.0001 ) THEN
      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)
         dpsdt(i,j)=0.
         dmudt(i,j)=0.
      ENDDO
      ENDDO
      ENDIF

   ENDDO
   !$OMP END PARALLEL DO

   END SUBROUTINE diagnostic_output_calc

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE clwrf_output_calc(                                      &
      ids,ide, jds,jde, kds,kde,                      &
      ims,ime, jms,jme, kms,kme,                      &
      ips,ipe, jps,jpe, kps,kpe,                      & ! patch  dims
      i_start,i_end,j_start,j_end,kts,kte,num_tiles   &
      ,clwrfH,t2,q2,u10,v10, skintemp                  & ! CLWRF
      ,t2clmin,t2clmax,tt2clmin,tt2clmax               & ! CLWRF
      ,t2clmean,t2clstd                                & ! CLWRF
      ,q2clmin,q2clmax,tq2clmin,tq2clmax               & ! CLWRF
      ,q2clmean,q2clstd                                & ! CLWRF
      ,u10clmax,v10clmax,spduv10clmax,tspduv10clmax    & ! CLWRF
      ,u10clmean,v10clmean,spduv10clmean               & ! CLWRF
      ,u10clstd,v10clstd,spduv10clstd                  & ! CLWRF
      ,raincclmax,rainncclmax,traincclmax,trainncclmax & ! CLWRF
      ,raincclmean,rainncclmean,raincclstd,rainncclstd & ! CLWRF
      ,skintempclmin,skintempclmax                     & ! CLWRF
      ,tskintempclmin,tskintempclmax                   & ! CLWRF
      ,skintempclmean,skintempclstd                    & ! CLWRF
      ,raincv,rainncv                                  &
      ,gswclmean,glwclmean,qfxclmean,olrclmean         & ! *SB*2012
      ,hfxclmean,lhclmean                              & ! *SB*2012
      ,gsw,glw,qfx,olr                                 & ! *SB*2012
      ,tlwdn, tlwup, slwdn, slwup                      & ! *SB*2013  ! for Goddard schemes
      ,tswdn, tswup, sswdn, sswup                      & ! *SB*2013  ! for Goddard schemes
      ,tlwdnmean, tlwupmean, slwdnmean, slwupmean      & ! *SB*2013  ! for Goddard schemes
      ,tswdnmean, tswupmean, sswdnmean, sswupmean      & ! *SB*2013  ! for Goddard schemes
      ,hfx,lh                                          & ! HFX, LH ja *SB*2012
      ,dt,xtime,curr_secs2                             &
      )
      !----------------------------------------------------------------------

!!$      USE module_configure 
!!$
      IMPLICIT NONE
      !======================================================================
      ! Definitions
      !-----------
      !-- DT            time step (second)
      !-- XTIME         forecast time
      !-- curr_secs2    current time in seconds since simulation restart
      !-- RAINCV        cumulus scheme precipitation in one time step (mm)
      !-- RAINNCV       explicit scheme precipitation in one time step (mm)
      !-- GSW           NET SHORT WAVE FLUX AT GROUND SURFACE     (W m-2)     ! *SB*2012
      !-- GLW           DOWNWARD LONG WAVE FLUX AT GROUND SURFACE (W m-2)     ! *SB*2012
      !-- QFX           UPWARD MOISTURE FLUX AT THE SURFACE       (kg m-2 s-1)! *SB*2012
      !-- OLR           TOA OUTGOING LONG WAVE                    (W m-2)     ! *SB*2012
      !-- HFX           surface sensible heat flux
      !-- LH            surface latent heat flux
      !-- U             u component of wind - to be used later to compute k.e.
      !-- V             v component of wind - to be used later to compute k.e.
      !
      !-- ids           start index for i in domain
      !-- ide           end index for i in domain
      !-- jds           start index for j in domain
      !-- jde           end index for j in domain
      !-- kds           start index for k in domain
      !-- kde           end index for k in domain
      !-- ims           start index for i in memory
      !-- ime           end index for i in memory
      !-- jms           start index for j in memory
      !-- jme           end index for j in memory
      !-- ips           start index for i in patch
      !-- ipe           end index for i in patch
      !-- jps           start index for j in patch
      !-- jpe           end index for j in patch
      !-- kms           start index for k in memory
      !-- kme           end index for k in memory
      !-- i_start       start indices for i in tile
      !-- i_end         end indices for i in tile
      !-- j_start       start indices for j in tile
      !-- j_end         end indices for j in tile
      !-- kts           start index for k in tile
      !-- kte           end index for k in tile
      !-- num_tiles     number of tiles
      !
      ! CLWRF-UC May.09 definitions
      !-----------
      ! is_restart: whether if simulation is a restart
      ! clwrfH: Interval (hour) of accumulation for computations 
      ! [var]cl[min/max]: [minimum/maximum] of variable [var] during interval
      ! t[var]cl[min/max]: Time (minutes) of [minimum/maximum] of variable 
      !    [var] during interval 
      ! [var]clmean: mean of variable [var] during interval
      ! [var]clstd: standard dev. of variable [var] during interval
      !    Variables are written on aux_hist_out7 (established
      !    in Registry)
      !
      !======================================================================

      INTEGER,      INTENT(IN   )                     ::            &
         ids,ide, jds,jde, kds,kde, &
         ims,ime, jms,jme, kms,kme, &
         ips,ipe, jps,jpe, kps,kpe, &
         kts,kte, &
         num_tiles

      INTEGER, DIMENSION(num_tiles), INTENT(IN)       :: i_start,   &
         i_end,j_start,j_end

      INTEGER, INTENT(IN)                       :: clwrfH
     REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN) ::           & 
         RAINNCV, RAINCV, HFX,      &
         LH, SKINTEMP 

      REAL,  INTENT(IN   )                            :: DT, XTIME
      REAL,  INTENT(IN   )                            :: curr_secs2

      !!-------------------
      !! CLWRF-UC Nov.09

      REAL, DIMENSION( ims:ime , jms:jme ),                                          & 
         INTENT(IN)         :: t2, q2, u10, v10 
      REAL, DIMENSION( ims:ime , jms:jme ),                                          &
         INTENT(OUT)        :: t2clmin, t2clmax, tt2clmin,       &
         tt2clmax, t2clmean, t2clstd,                            & 
         q2clmin, q2clmax, tq2clmin, tq2clmax, q2clmean, q2clstd,&
         u10clmax, v10clmax, spduv10clmax, tspduv10clmax,        &
         u10clmean, v10clmean, spduv10clmean,                    &
         u10clstd, v10clstd, spduv10clstd, skintempclmin,        &
         skintempclmax, tskintempclmin, tskintempclmax,          &
         skintempclmean, skintempclstd
      REAL, DIMENSION( ims:ime , jms:jme ),                                          &
         INTENT(OUT)        :: raincclmax, rainncclmax,          &
         traincclmax, trainncclmax, raincclmean, rainncclmean,   & 
         raincclstd, rainncclstd 
      REAL, DIMENSION( ims:ime , jms:jme ),                                          & ! *SB*2012  
         INTENT(IN)         :: gsw,glw,qfx,olr                     ! *SB*2012 
      REAL, DIMENSION( ims:ime , jms:jme ),                                          & ! *SB*2012 
         INTENT(OUT)        :: gswclmean,glwclmean,qfxclmean,    & ! *SB*2012
         olrclmean,hfxclmean,lhclmean                              ! *SB*2012
      ! Optional, only for Goddard LW and SW
      REAL, DIMENSION(ims:ime, jms:jme), OPTIONAL, INTENT(IN)  ::           & ! *SB*2013 
         tlwdn, tlwup,             & ! *SB*2013 
         slwdn, slwup,             & ! *SB*2013 
         tswdn, tswup,             & ! *SB*2013 
         sswdn, sswup                ! *SB*2013        
      REAL, DIMENSION(ims:ime, jms:jme), OPTIONAL, INTENT(OUT) ::           & ! *SB*2013 
         tlwdnmean, tlwupmean,     & ! *SB*2013 
         slwdnmean, slwupmean,     & ! *SB*2013 
         tswdnmean, tswupmean,     & ! *SB*2013 
         sswdnmean, sswupmean        ! for Goddard schemes

      ! LOCAL  VAR

      INTEGER                                   :: i,j,ij
      REAL                                      :: xtimep
      LOGICAL, EXTERNAL                         :: wrf_dm_on_monitor
      REAL, PARAMETER                           :: minimum0= 1000000.,maximum0= -1000000. 
      CHARACTER (LEN=1024)                      :: message
      REAL, SAVE                                :: nsteps,r_nstep
      REAL                                      :: zfac
      INTEGER                                   :: ii,jj

      !-----------------------------------------------------------------
      ! Compute minutes from reference times clwrfH

      ! Initialize [var] values
      ! SET START AND END POINTS FOR TILES
      zfac=60./dt
      IF ( MOD(NINT(curr_secs2/dt),NINT(REAL(clwrfH)*zfac)) == 0 ) THEN
         !  !$OMP PARALLEL DO   &
         !  !$OMP PRIVATE ( ij )
         DO ij = 1 , num_tiles
            IF  ( wrf_dm_on_monitor() ) THEN
               ii=i_start(ij)+(i_end(ij)-i_start(ij))/2
               jj=j_start(ij)+(j_end(ij)-j_start(ij))/2
               CALL blabla(       'T2', ij,        t2clmin(ii,jj),        t2clmax(ii,jj),       tt2clmin(ii,jj)   &
                  &                       ,       tt2clmax(ii,jj),       t2clmean(ii,jj),        t2clstd(ii,jj) )
               CALL blabla(       'Q2', ij,        q2clmin(ii,jj),        q2clmax(ii,jj),       tq2clmin(ii,jj)   &
                  &                       ,       tq2clmax(ii,jj),       q2clmean(ii,jj),        q2clstd(ii,jj) )
               CALL blabla( 'SKINTEMP', ij,  skintempclmin(ii,jj),  skintempclmax(ii,jj), tskintempclmin(ii,jj)   &
                  &                       , tskintempclmax(ii,jj), skintempclmean(ii,jj),  skintempclstd(ii,jj) )

               CALL blablaMAX(     'U10', ij,     u10clmax(ii,jj), tspduv10clmax(ii,jj),     u10clmean(ii,jj),     u10clstd(ii,jj) )
               CALL blablaMAX(     'V10', ij,     v10clmax(ii,jj), tspduv10clmax(ii,jj),     v10clmean(ii,jj),     v10clstd(ii,jj) )
               CALL blablaMAX( 'SPDUV10', ij, spduv10clmax(ii,jj), tspduv10clmax(ii,jj), spduv10clmean(ii,jj), spduv10clstd(ii,jj) )
               CALL blablaMAX(   'RAINC', ij,   raincclmax(ii,jj),   traincclmax(ii,jj),   raincclmean(ii,jj),   raincclstd(ii,jj) )
               CALL blablaMAX(  'RAINNC', ij,  rainncclmax(ii,jj),  trainncclmax(ii,jj),  rainncclmean(ii,jj),  rainncclstd(ii,jj) )
            ENDIF
            DO j = j_start(ij), j_end(ij)
               DO i = i_start(ij), i_end(ij)
                  t2clmin(i,j)=minimum0
                  t2clmax(i,j)=maximum0
                  t2clmean(i,j)=0.0
                  t2clstd(i,j)=0.0
                  q2clmin(i,j)=minimum0
                  q2clmax(i,j)=maximum0
                  q2clmean(i,j)=0.0
                  q2clstd(i,j)=0.0
                  spduv10clmax(i,j)=0.0
                  u10clmean(i,j)=0.0
                  v10clmean(i,j)=0.0
                  spduv10clmean(i,j)=0.0
                  u10clstd(i,j)=0.0
                  v10clstd(i,j)=0.0
                  spduv10clstd(i,j)=0.0
                  raincclmax(i,j)=0.0
                  rainncclmax(i,j)=0.0
                  raincclmean(i,j)=0.0
                  rainncclmean(i,j)=0.0
                  raincclstd(i,j)=0.0
                  rainncclstd(i,j)=0.0
                  skintempclmin(i,j)=minimum0
                  skintempclmax(i,j)=maximum0
                  skintempclmean(i,j)=0.0
                  skintempclstd(i,j)=0.0
                  gswclmean(i,j)=0.0              ! *SB*2012
                  glwclmean(i,j)=0.0              ! *SB*2012
                  qfxclmean(i,j)=0.0              ! *SB*2012
                  olrclmean(i,j)=0.0              ! *SB*2012
                  hfxclmean(i,j)=0.0              ! *SB*2012
                  lhclmean(i,j)=0.0               ! *SB*2012
                  tlwdnmean(i,j)=0.0              ! *SB*2013  ! for Goddard schemes 
                  tlwupmean(i,j)=0.0              ! *SB*2013  ! for Goddard schemes 
                  slwdnmean(i,j)=0.0              ! *SB*2013  ! for Goddard schemes 
                  slwupmean(i,j)=0.0              ! *SB*2013  ! for Goddard schemes
                  tswdnmean(i,j)=0.0              ! *SB*2013  ! for Goddard schemes 
                  tswupmean(i,j)=0.0              ! *SB*2013  ! for Goddard schemes 
                  sswdnmean(i,j)=0.0              ! *SB*2013  ! for Goddard schemes 
                  sswupmean(i,j)=0.0              ! *SB*2013  ! for Goddard schemes
               ENDDO
            ENDDO
         ENDDO
         !  !$OMP END PARALLEL DO
      ENDIF

      xtimep = xtime + dt/60.   ! value at end of timestep for time info
      nsteps = REAL(NINT(REAL(clwrfH)*60./dt))
      r_nstep = 1./nsteps
      ! Temperature
      CALL varstatistics(t2-273.15, r_nstep, xtimep, t2clmin, t2clmax, tt2clmin, tt2clmax, t2clmean, t2clstd,   &
         &               ims, ime, jms, jme, i_start, i_end, j_start, j_end, num_tiles ) 
      ! Water vapor mixing ratio
      CALL varstatistics(q2, r_nstep,xtimep,q2clmin,q2clmax,tq2clmin,tq2clmax,q2clmean,q2clstd,   &
         &               ims, ime, jms, jme, i_start, i_end, j_start, j_end, num_tiles ) 
      ! Skin Temperature 
      CALL varstatistics(skintemp-273.15,r_nstep,xtimep,skintempclmin,skintempclmax, tskintempclmin,   &
         &                                        tskintempclmax,skintempclmean,skintempclstd ,   &
         &               ims, ime, jms, jme, i_start, i_end, j_start, j_end, num_tiles ) 
      ! Wind speed
      CALL varstatisticsWIND(u10,v10,r_nstep,xtimep,u10clmax,u10clmean,u10clstd,v10clmax,v10clmean,v10clstd, &
         &                   spduv10clmax,tspduv10clmax,spduv10clmean,spduv10clstd,   &
         &                   ims, ime, jms, jme, i_start, i_end, j_start, j_end, num_tiles )
      ! Precipitation flux
      CALL varstatisticsMAX(raincv/dt, r_nstep, xtimep, raincclmax, traincclmax, raincclmean, raincclstd,   &
         &                  ims, ime, jms, jme, i_start, i_end, j_start, j_end, num_tiles ) 
      CALL varstatisticsMAX(rainncv/dt, r_nstep, xtimep, rainncclmax, trainncclmax, rainncclmean, rainncclstd,   &
         &                  ims, ime, jms, jme, i_start, i_end, j_start, j_end, num_tiles ) 
      ! Radiation var 
      CALL varstatisticsMEAN(gsw, r_nstep, gswclmean, ims, ime, jms, jme, i_start, i_end, j_start, j_end, num_tiles)
      CALL varstatisticsMEAN(glw, r_nstep, glwclmean, ims, ime, jms, jme, i_start, i_end, j_start, j_end, num_tiles)
      CALL varstatisticsMEAN(qfx, r_nstep, qfxclmean, ims, ime, jms, jme, i_start, i_end, j_start, j_end, num_tiles)
      CALL varstatisticsMEAN(olr, r_nstep, olrclmean, ims, ime, jms, jme, i_start, i_end, j_start, j_end, num_tiles)
      CALL varstatisticsMEAN(hfx, r_nstep, hfxclmean, ims, ime, jms, jme, i_start, i_end, j_start, j_end, num_tiles)
      CALL varstatisticsMEAN( lh, r_nstep,  lhclmean, ims, ime, jms, jme, i_start, i_end, j_start, j_end, num_tiles)
      ! for Goddard schemes 
      CALL varstatisticsMEAN(tlwdn, r_nstep, tlwdnmean, ims, ime, jms, jme, i_start, i_end, j_start, j_end, num_tiles)
      CALL varstatisticsMEAN(tlwup, r_nstep, tlwupmean, ims, ime, jms, jme, i_start, i_end, j_start, j_end, num_tiles)
      CALL varstatisticsMEAN(slwdn, r_nstep, slwdnmean, ims, ime, jms, jme, i_start, i_end, j_start, j_end, num_tiles)
      CALL varstatisticsMEAN(slwup, r_nstep, slwupmean, ims, ime, jms, jme, i_start, i_end, j_start, j_end, num_tiles)
      CALL varstatisticsMEAN(tswdn, r_nstep, tswdnmean, ims, ime, jms, jme, i_start, i_end, j_start, j_end, num_tiles)
      CALL varstatisticsMEAN(tswup, r_nstep, tswupmean, ims, ime, jms, jme, i_start, i_end, j_start, j_end, num_tiles)
      CALL varstatisticsMEAN(sswdn, r_nstep, sswdnmean, ims, ime, jms, jme, i_start, i_end, j_start, j_end, num_tiles)
      CALL varstatisticsMEAN(sswup, r_nstep, sswupmean, ims, ime, jms, jme, i_start, i_end, j_start, j_end, num_tiles)

      !          IF (MOD(NINT(XTIME),clwrfH) == 0) THEN
      !          IF (MOD(NINT(XTIME+dt/60.),clwrfH) == 0) THEN
      IF ((MOD(NINT((curr_secs2+dt)/dt),NINT(REAL(clwrfH)*zfac)) == 0)) THEN
         IF  ( wrf_dm_on_monitor() ) PRINT *,'nsteps=',nsteps,' xtime:',  xtime,' clwrfH:',clwrfH,' dt:', dt
         !  !$OMP PARALLEL DO   &
         !  !$OMP PRIVATE ( ij )
         DO ij = 1 , num_tiles
            DO j = j_start(ij), j_end(ij)
               DO i = i_start(ij), i_end(ij)
                  t2clstd(i,j)       = SQRT(      t2clstd(i,j) -       t2clmean(i,j) *       t2clmean(i,j))
                  q2clstd(i,j)       = SQRT(      q2clstd(i,j) -       q2clmean(i,j) *       q2clmean(i,j))
                  skintempclstd(i,j) = SQRT(skintempclstd(i,j) - skintempclmean(i,j) * skintempclmean(i,j))
                  u10clstd(i,j)      = SQRT(     u10clstd(i,j) -      u10clmean(i,j) *      u10clmean(i,j))
                  v10clstd(i,j)      = SQRT(     v10clstd(i,j) -      v10clmean(i,j) *      v10clmean(i,j))
                  spduv10clstd(i,j)  = SQRT( spduv10clstd(i,j) -  spduv10clmean(i,j) *  spduv10clmean(i,j))
                  raincclstd(i,j)    = SQRT(   raincclstd(i,j) -    raincclmean(i,j) *    raincclmean(i,j))
                  rainncclstd(i,j)   = SQRT(  rainncclstd(i,j) -   rainncclmean(i,j) *   rainncclmean(i,j))
               ENDDO
            ENDDO
         ENDDO
         !  !$OMP END PARALLEL DO
      END IF

   END SUBROUTINE clwrf_output_calc

   SUBROUTINE varstatisticsWIND(varu, varv, r_nstep, tt                 &
      &                   , varumax ,            varumean , varustd     &
      &                   , varvmax ,            varvmean , varvstd     &
      &                   , varuvmax, tvaruvmax, varuvmean, varuvstd    &
      &                   , ims, ime, jms, jme, i_start, i_end, j_start, j_end, num_tiles ) 
      ! Subroutine to compute variable statistics

      IMPLICIT NONE

      REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN   ) :: varu, varv
      REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: varumax ,            varumean , varustd
      REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: varvmax ,            varvmean , varvstd
      REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: varuvmax, tvaruvmax, varuvmean, varuvstd
      REAL                             , INTENT(IN   ) :: r_nstep
      REAL                             , INTENT(IN   ) :: tt
      INTEGER                          , INTENT(IN   ) :: ims,ime, jms,jme
      INTEGER                          , INTENT(IN   ) :: num_tiles
      INTEGER, DIMENSION(num_tiles)    , INTENT(IN   ) :: i_start, i_end, j_start, j_end

      INTEGER :: ij, i,j
      REAL :: ztmpu,ztmpu2,ztmpv,ztmpv2,ztmpuv,ztmpuv2

      !  !$OMP PARALLEL DO   &
      !  !$OMP PRIVATE ( ij )
      DO ij = 1 , num_tiles
         DO j = j_start(ij), j_end(ij)
            DO i = i_start(ij), i_end(ij)
               ztmpu = varu(i,j)
               ztmpv = varv(i,j)
               ztmpuv=SQRT(ztmpu*ztmpu+ztmpv*ztmpv)
               IF (ztmpuv > varuvmax(i,j)) THEN
                  varumax(i,j)=ztmpu
                  varvmax(i,j)=ztmpv
                  varuvmax(i,j)=ztmpuv
                  tvaruvmax(i,j)=tt
               END IF
               ztmpu2 = ztmpu * r_nstep
               varumean(i,j) = varumean(i,j) + ztmpu2
               varustd(i,j) = varustd(i,j) + ztmpu2 * ztmpu
               ztmpv2 = ztmpv * r_nstep
               varvmean(i,j) = varvmean(i,j) + ztmpv2
               varvstd(i,j) = varvstd(i,j) + ztmpv2 * ztmpv
               ztmpuv2 = ztmpuv * r_nstep
               varuvmean(i,j) = varuvmean(i,j) + ztmpuv2
               varuvstd(i,j) = varuvstd(i,j) + ztmpuv2 * ztmpuv
            ENDDO
         ENDDO
      ENDDO
      !  !$OMP END PARALLEL DO

   END SUBROUTINE varstatisticsWIND

   SUBROUTINE varstatisticsMEAN(var, r_nstep, varmean, ims, ime, jms, jme, i_start, i_end, j_start, j_end, num_tiles ) 
      ! Subroutine to compute variable statistics

      IMPLICIT NONE

      REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN   ) :: var
      REAL, DIMENSION(ims:ime, jms:jme), INTENT(  OUT) :: varmean
      REAL                             , INTENT(IN   ) :: r_nstep
      INTEGER                          , INTENT(IN   ) :: ims,ime, jms,jme
      INTEGER                          , INTENT(IN   ) :: num_tiles
      INTEGER, DIMENSION(num_tiles)    , INTENT(IN   ) :: i_start, i_end, j_start, j_end

      INTEGER :: ij, i,j

      !  !$OMP PARALLEL DO   &
      !  !$OMP PRIVATE ( ij )
      DO ij = 1 , num_tiles
         DO j = j_start(ij), j_end(ij)
            DO i = i_start(ij), i_end(ij)
               varmean(i,j) = varmean(i,j) + var(i,j) * r_nstep
            ENDDO
         ENDDO
      ENDDO
      !  !$OMP END PARALLEL DO

   END SUBROUTINE varstatisticsMEAN

   SUBROUTINE varstatisticsMAX(var, r_nstep, tt, varmax, tvarmax,varmean, varstd  &
      &                   , ims, ime, jms, jme, i_start, i_end, j_start, j_end, num_tiles ) 
      ! Subroutine to compute variable statistics

      IMPLICIT NONE

      REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN   ) :: var
      REAL, DIMENSION(ims:ime, jms:jme), INTENT(  OUT) :: varmax, tvarmax, varmean, varstd
      REAL                             , INTENT(IN   ) :: r_nstep
      REAL                             , INTENT(IN   ) :: tt
      INTEGER                          , INTENT(IN   ) :: ims,ime, jms,jme
      INTEGER                          , INTENT(IN   ) :: num_tiles
      INTEGER, DIMENSION(num_tiles)    , INTENT(IN   ) :: i_start, i_end, j_start, j_end

      INTEGER :: ij, i,j
      REAL :: ztmp,ztmp2

      !  !$OMP PARALLEL DO   &
      !  !$OMP PRIVATE ( ij )
      DO ij = 1 , num_tiles
         DO j = j_start(ij), j_end(ij)
            DO i = i_start(ij), i_end(ij)
               ztmp = var(i,j)
               IF (ztmp > varmax(i,j)) THEN
                  varmax(i,j)=ztmp
                  tvarmax(i,j)=tt
               END IF
               ztmp2 = ztmp * r_nstep
               varmean(i,j) = varmean(i,j) + ztmp2
               varstd(i,j) = varstd(i,j) + ztmp2 * ztmp
            ENDDO
         ENDDO
      ENDDO
      !  !$OMP END PARALLEL DO

   END SUBROUTINE varstatisticsMAX

   SUBROUTINE varstatistics(var, r_nstep, tt, varmin, varmax, tvarmin, tvarmax,varmean, varstd  &
      &                   , ims, ime, jms, jme, i_start, i_end, j_start, j_end, num_tiles ) 
      ! Subroutine to compute variable statistics

      IMPLICIT NONE

      REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN   ) :: var
      REAL, DIMENSION(ims:ime, jms:jme), INTENT(  OUT) :: varmin, varmax, tvarmin, tvarmax, varmean, varstd
      REAL                             , INTENT(IN   ) :: r_nstep
      REAL                             , INTENT(IN   ) :: tt
      INTEGER                          , INTENT(IN   ) :: ims,ime, jms,jme
      INTEGER                          , INTENT(IN   ) :: num_tiles
      INTEGER, DIMENSION(num_tiles)    , INTENT(IN   ) :: i_start, i_end, j_start, j_end

      INTEGER :: ij, i,j
      REAL :: ztmp,ztmp2

      !  !$OMP PARALLEL DO   &
      !  !$OMP PRIVATE ( ij )
      DO ij = 1 , num_tiles
         DO j = j_start(ij), j_end(ij)
            DO i = i_start(ij), i_end(ij)
               ztmp = var(i,j)
               IF (ztmp < varmin(i,j)) THEN
                  varmin(i,j)=ztmp
                  tvarmin(i,j)=tt
               END IF
               IF (ztmp > varmax(i,j)) THEN
                  varmax(i,j)=ztmp
                  tvarmax(i,j)=tt
               END IF
               ztmp2 = ztmp * r_nstep
               varmean(i,j) = varmean(i,j) + ztmp2
               varstd(i,j) = varstd(i,j) + ztmp2 * ztmp
            ENDDO
         ENDDO
      ENDDO
      !  !$OMP END PARALLEL DO

   END SUBROUTINE varstatistics

   SUBROUTINE blabla(varname,ij,vmin,vmax,tvmin,tvmax,vmean,vstd)

      IMPLICIT NONE

      CHARACTER(*), INTENT(IN) :: varname
      INTEGER     , INTENT(IN) :: ij
      REAL        , INTENT(IN) :: vmin,vmax,tvmin,tvmax,vmean,vstd

      CHARACTER(LEN=1024) :: message

      WRITE(message, *)'CLWRFdiag - '//TRIM(varname)//'; tile: ',ij &
         &  ,'  '//TRIM(varname)//'clmin : ', vmin ,'  '//TRIM(varname)//'clmax: ', vmax    &
         &  ,' T'//TRIM(varname)//'clmin : ',tvmin ,' T'//TRIM(varname)//'clmax: ',tvmax    &
         &  ,'  '//TRIM(varname)//'clmean: ', vmean,'  '//TRIM(varname)//'clstd: ', vstd
      CALL wrf_debug(75, message)

   END SUBROUTINE blabla

   SUBROUTINE blablaMAX(varname,ij,vmax,tvmax,vmean,vstd)

      IMPLICIT NONE

      CHARACTER(*), INTENT(IN) :: varname
      INTEGER     , INTENT(IN) :: ij
      REAL        , INTENT(IN) :: vmax,tvmax,vmean,vstd

      CHARACTER(LEN=1024) :: message

      WRITE(message, *)'CLWRFdiag - '//TRIM(varname)//'; tile: ',ij &
         &  ,'  '//TRIM(varname)//'clmax : ', vmax ,' T'//TRIM(varname)//'clmax: ',tvmax    &
         &  ,'  '//TRIM(varname)//'clmean: ', vmean,'  '//TRIM(varname)//'clstd: ', vstd
      CALL wrf_debug(75, message)

   END SUBROUTINE blablaMAX

END MODULE module_diagnostics
