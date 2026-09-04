      module netcdf_parms
        type netcdfstuff  ! Define a new type for NetCDF information
          ! All of these "name" variables are the names for the 
          ! different variables in the NetCDF file.
          character*180 :: netcdf_filename ! character file name for 
                                           ! the NetCDF file.
          character*30 ::  time_name  ! Name of time variable,
                                      ! usually "time"
          character*30 ::  lon_name   ! longitudes
          character*30 ::  lat_name   ! latitudes
          character*30 ::  time_units ! "days" or "hours"
        end type netcdfstuff
        real, save, allocatable :: netcdf_file_time_values(:)
        integer, save, allocatable :: nctotalmins(:)
        integer, save, allocatable :: nctotalhours(:)
      end module netcdf_parms
c
c-----------------------------------------------------------------------
      program tave
c
c     ABSTRACT: This program averages the temperatures from an input 
c     grib file and produces an output grib file containing the mean
c     temperature in the 300-500 mb layer.  For each model and each
c     lead time, there will need to be data from 300 to 500 mb in 
c     50 mb increments, such that all 5 of these layers then get
c     averaged together.
c
c     Written by Tim Marchok

      USE params
      USE grib_mod

      implicit none

      type(gribfield) :: holdgfld
      type(netcdfstuff) :: netcdfinfo
c
      integer, parameter :: lugb=11,lulv=16,lugi=31,lout=51
      integer, parameter :: lunvn=12
      integer, parameter :: nlevsout=1,nlevsin=5
      integer  kpds(200),kgds(200)
      integer  iriret,iogret,kf,iggret,igdret,iidret,gribver,g2_jpdtn
      integer  iha,iho,iva,irfa,iodret,ifcsthour,iia,iparm,ncfile_id
      integer  ignvret,ignret,ifhmax,ncfile_tmax
      integer  ilevs(nlevsin)
      integer date_time(8)
      character (len=10) big_ben(3)
      character*1 :: ncfile_has_hour0
      character*6 :: inp_data_type ! Has a value of 'grib' or 'netcdf'
      character*180 :: netcdf_filename ! character file name for
                                       ! the NetCDF file.
      character*180 :: ncfile
      character*60 :: cnvname_300,cnvname_350,cnvname_400
      character*60 :: cnvname_450,cnvname_500
      real, allocatable :: xinptmp(:,:),xouttmp(:)
      logical(1), allocatable :: valid_pt(:),readflag(:)
      real     xoutlev

      namelist/timein/ifcsthour,iparm,gribver,g2_jpdtn,inp_data_type
     &               ,netcdf_filename
c
      data ilevs   /300, 350, 400, 450, 500/
      xoutlev = 401.
c
      cnvname_300 = 'xxxxx'
      cnvname_350 = 'xxxxx'
      cnvname_400 = 'xxxxx'
      cnvname_450 = 'xxxxx'
      cnvname_500 = 'xxxxx'
      ncfile_has_hour0 = 'n'
c
      read (5,NML=timein,END=201)
  201 continue
      print *,' '
      print *,'*---------------------------------------------*'
      print *,' '
      print *,' +++ Top of tave +++ '
      print *,' '
      print *,'After tave namelist read, input forecast hour= '
     &       ,ifcsthour
      print *,'                         input GRIB parm= ',iparm
      print *,'                         GRIB version=    ',gribver
      print *,'                         GRIB2 JPDTN= g2_jpdtn= '
     &                                              ,g2_jpdtn
      print *,'                         inp_data_type=   '
     &                                              ,inp_data_type
      print *,'                         netcdf_filename= '
     &                                              ,netcdf_filename

c      ilevs = -999
c      call read_input_levels (lulv,nlevsin,ilevs,iriret)
c
c      if (iriret /= 0) then
c        print *,' '
c        print *,'!!! RETURN CODE FROM read_input_levels /= 0'
c        print *,'!!! RETURN CODE = iriret = ',iriret
c        print *,'!!! EXITING....'
c        print *,' '
c        goto 899
c      endif

      call date_and_time (big_ben(1),big_ben(2),big_ben(3),date_time)
      write (6,31) date_time(5),date_time(6),date_time(7)
 31   format (1x,'TIMING: b4 open_grib_files ',i2.2,':',i2.2,':',i2.2)

      if (inp_data_type == 'grib') then
        call open_grib_files (lugb,lugi,lout,gribver,iogret)
        if (iogret /= 0) then
          print '(/,a35,a5,i4,/)','!!! ERROR: in tave open_grib_files,'
     &          ,' rc= ',iogret
          goto 899
        endif
      elseif (inp_data_type == 'netcdf') then
        ncfile = netcdfinfo%netcdf_filename
        print *,' '
        print *,'before open_ncfile call, ncfile= ',ncfile
        call open_ncfile (ncfile,ncfile_id)
        print *,'after open_ncfile call, ncfile_id= ',ncfile_id
        call get_netcdf_varnames (lunvn,cnvname_300,cnvname_350
     &             ,cnvname_400,cnvname_450,cnvname_500,ignvret)
      else
        print *,' '
        print *,'ERROR: inp_data_type in tave not recognized!!'
        print *,'       inp_data_type= ',inp_data_type
        stop 99
      endif

      call date_and_time (big_ben(1),big_ben(2),big_ben(3)
     &                   ,date_time)
      write (6,32) date_time(5),date_time(6),date_time(7)
 32   format (1x,'TIMING: after open_grib_files ',i2.2,':',i2.2
     &        ,':',i2.2)


      if (inp_data_type == 'grib') then
        call getgridinfo_grib (lugb,lugi,kf,kpds,kgds,holdgfld
     &                 ,ifcsthour,iparm,gribver,g2_jpdtn,iggret)
      else
        call getgridinfo_netcdf (ncfile_id,imax,jmax,kf,netcdfinfo
     &                 ,iggret)
      endif

      call date_and_time (big_ben(1),big_ben(2),big_ben(3),date_time)
      write (6,33) date_time(5),date_time(6),date_time(7)
 33   format (1x,'TIMING: after getgridinfo ',i2.2,':',i2.2
     &        ,':',i2.2)

      if (inp_data_type == 'netcdf') then
        call read_netcdf_hours (ncfile,ncfile_id,ncfile_tmax,ifhmax
     &                         ,ncfile_has_hour0,netcdfinfo,irnhret)
      endif

      allocate (xinptmp(kf,nlevsin),stat=iha)
      allocate (xouttmp(kf),stat=iho)
      allocate (valid_pt(kf),stat=iva)
      allocate (readflag(nlevsin),stat=irfa)
      if (iha /= 0 .or. iho /= 0 .or. iva /= 0 .or. irfa /= 0) then
        print *,' '
        print *,'!!! ERROR in tave allocating arrays.'
        print *,'!!! ERROR allocating the xinptmp, readflag, or the'
        print *,'!!! valid_pt array, iha= ',iha,' iva= ',iva
        print *,'!!! irfa= ',irfa,' iho= ',iho
        print *,'  '
        goto 899
      endif

      if (inp_data_type == 'grib') then
        call getdata_grib (lugb,lugi,kf,valid_pt,nlevsin,ilevs
     &               ,readflag,xinptmp,ifcsthour,iparm,gribver
     &               ,g2_jpdtn,igdret)
      else
        call getdata_netcdf (ncfile_id,imax,jmax,kf,readflag,xinptmp
     &               ,ifcsthour,ifh,ncfile_tmax,netcdfinfo,nlevsin
     &               ,cnvname_300,cnvname_350,cnvname_400
     &               ,cnvname_450,cnvname_500,valid_pt,ignret)
      endif

      call date_and_time (big_ben(1),big_ben(2),big_ben(3),date_time)
      write (6,34) date_time(5),date_time(6),date_time(7)
 34   format (1x,'TIMING: after getdata ',i2.2,':',i2.2
     &        ,':',i2.2)

      call average_data (kf,valid_pt,nlevsin,ilevs,readflag
     &                  ,xinptmp,xouttmp,iidret)

      call date_and_time (big_ben(1),big_ben(2),big_ben(3),date_time)
      write (6,35) date_time(5),date_time(6),date_time(7)
 35   format (1x,'TIMING: after average_data ',i2.2,':',i2.2
     &        ,':',i2.2)

      call output_data (lout,kf,kpds,kgds,holdgfld,xouttmp,valid_pt
     &                ,xoutlev,nlevsout,gribver,ifcsthour,iodret)

      call date_and_time (big_ben(1),big_ben(2),big_ben(3),date_time)
      write (6,36) date_time(5),date_time(6),date_time(7)
 36   format (1x,'TIMING: after output_data ',i2.2,':',i2.2
     &        ,':',i2.2)

      call gf_free (holdgfld)

      deallocate (xinptmp)
      deallocate (xouttmp)
      deallocate (valid_pt)
      deallocate (readflag)

  899 continue
c 
      stop
      end
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine read_input_levels (lulv,nlevsin,ilevs,iriret)
c 
c     ABSTRACT: This subroutine reads in a text file that contains
c     the number of input pressure levels for a given model.  The
c     format of the file goes like this, from upper levels to 
c     lower, for example:
c
c        1       200
c        2       400
c        3       500
c        4       700
c        5       850
c        6       925
c        7      1000
c
c
      implicit none

      integer    lulv,nlevsin,iriret,inplev,ict,lvix
      integer    ilevs(nlevsin)
c
      iriret=0
      ict = 0
      do while (.true.)
        
        print *,'Top of while loop in tave read_input_levels'

        read (lulv,85,end=130) lvix,inplev

        if (inplev > 0 .and. inplev <= 1000) then
          ict = ict + 1
          ilevs(ict) = inplev
        else
          print *,' '
          print *,'!!! ERROR: Input level not between 0 and 1000'
          print *,'!!!        in tave.  inplev= ',inplev
          print *,'!!! STOPPING EXECUTION'
          STOP 91
        endif

        print *,'tave readloop, ict= ',ict,' inplev= ',inplev

      enddo

   85 format (i4,1x,i4)
  130 continue

      nlevsin = ict

      print *,' '
      print *,'Total number of tave levels read in = ',nlevsin
c 
      return
      end
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine get_netcdf_varnames (lunvn,cnvname_300,cnvname_350
     &                 ,cnvname_400,cnvname_450,cnvname_500,ignvret)
c
c     ABSTRACT: The purpose of this subroutine is to read a text file
c     and grab the names of the NetCDF variables that are used for 
c     temperature at 300, 350, 400, 450 and 500 mb.  Obviously, this 
c     routine is only called if the input data format is NetCDF.
c
      implicit none
c
      integer :: lunvn,ignvret,ict,inplev
      character*60 :: cnvname_300,cnvname_350,cnvname_400
      character*60 :: cnvname_450,cnvname_500,cstr
c
      ignvret=0
      ict = 0

      do ict = 1,5
      
        print *,'In loop in tave get_netcdf_varnames, ict= ',ict

        read (lunvn,85,end=130) inplev,cstr
      
        if (inplev > 0 .and. inplev <= 1000) then
          ict = ict + 1
          ilevs(ict) = inplev
        else
          print *,' '
          print *,'!!! ERROR: Input level not between 0 and 1000'
          print *,'!!!        in tave.  inplev= ',inplev
          print *,'!!! STOPPING EXECUTION'
          STOP 91
        endif 

        select case (inplev)
          case (300); cnvname_300 = str
          case (350); cnvname_350 = str
          case (400); cnvname_400 = str
          case (450); cnvname_450 = str
          case (500); cnvname_500 = str
        end select
      
        print *,' '
        print *,'netcdf varname readloop, ict= ',ict
        print *,'   inplev=      ',inplev
        print *,'   str=         ',str
        print *,'   cnvname_300= ',cnvname_300
        print *,'   cnvname_350= ',cnvname_350
        print *,'   cnvname_400= ',cnvname_400
        print *,'   cnvname_450= ',cnvname_450
        print *,'   cnvname_500= ',cnvname_500

      enddo

   85 format (i4,1x,a60)
  130 continue

      return
      end
c 
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine getgridinfo_grib (lugb,lugi,kf,kpds,kgds,holdgfld
     &                    ,ifcsthour,iparm,gribver,g2_jpdtn,iggret)
c
c     ABSTRACT: The purpose of this subroutine is just to get the max
c     values of i and j and the dx and dy grid spacing intervals for the
c     grid to be used in the rest of the program.  So just read the
c     grib file to get the lon and lat data.  Also, get the info for
c     the data grids boundaries.  
c
C     INPUT:
C     lugb     The Fortran unit number for the GRIB data file
C     lugi     The Fortran unit number for the GRIB index file
c     ifcsthour input forecast hour to search for
c     iparm    input grib parm to search for
c     gribver  integer (1 or 2) to indicate if using GRIB1 / GRIB2
c     g2_jpdtn If GRIB2 data being read, this is the value for JPDTN
c              that is input to getgb2.
C
C     OUTPUT:
c     kf       Number of gridpoints on the grid
c     kpds     pds array for a GRIB1 record
c     kgds     gds array for a GRIB1 record
c     holdgfld info for a GRIB2 record
c     
C     iggret   The return code from this subroutine
c
      USE params
      USE grib_mod

      implicit none
c
      CHARACTER(len=8) :: ctemp
      CHARACTER(len=80) :: ftemplate
      type(gribfield) :: holdgfld
      integer,dimension(200) :: jids,jpdt,jgdt
      logical(1), allocatable :: lb(:)
      integer, parameter :: jf=40000000
      integer   jpds(200),jgds(200)
      integer   kpds(200),kgds(200)
      integer :: listsec1(13)
      integer   ila,ifa,iret,ifcsthour,imax,jmax,jskp,jdisc
      integer   lugb,lugi,kf,j,k,iggret,iparm,gribver,g2_jpdtn
      integer   jpdtn,jgdtn,npoints,icount,ipack,krec
      integer :: listsec0(2)=(/0,2/)
      integer :: igds(5)=(/0,0,0,0,0/),previgds(5)
      integer :: idrstmpl(200)
      integer :: currlen=1000000
      logical :: unpack=.true.
      logical :: open_grb=.false.
      real, allocatable :: f(:)
      real      dx,dy
c
      iggret = 0

      allocate (lb(jf),stat=ila) 
      allocate (f(jf),stat=ifa)
      if (ila /= 0 .or. ifa /= 0) then
        print *,' '
        print *,'!!! ERROR in tave.'
        print *,'!!! ERROR in getgridinfo allocating either lb or f'
        print *,'!!! ila = ',ila,' ifa= ',ifa
        iggret = 97
        return
      endif

      if (gribver == 2) then

        ! Search for a record from a GRIB2 file

        !
        ! ---  Initialize Variables ---
        !

        holdgfld%idsect => NULL()
        holdgfld%local => NULL()
        holdgfld%list_opt => NULL()
        holdgfld%igdtmpl => NULL()
        holdgfld%ipdtmpl => NULL()
        holdgfld%coord_list => NULL()
        holdgfld%idrtmpl => NULL()
        holdgfld%bmap => NULL()
        holdgfld%fld => NULL()

        jdisc=0  ! Meteorological products
        jids=-9999
        jpdtn=g2_jpdtn  ! 0 = analysis or forecast; 1 = ens fcst
        jgdtn=0  ! lat/lon grid
        jgdt=-9999
        jpdt=-9999

        npoints=0
        icount=0
        jskp=0

c       Search for Temperature by production template 4.0

        JPDT(1:15)=(/ -9999,-9999,-9999,-9999,-9999,-9999,-9999,-9999
     &             ,-9999,-9999,-9999,-9999,-9999,-9999,-9999/)

c       We need to be sure that we search for a temperature record,
c       and we do that by specifying jpdt(1) and jpdt(2) with the 
c       values of the product definition template parameter category
c       (NCEP GRIB2 Table 4.1, where the indicator for temperature = 0),
c       and the product definition template parameter number
c       (NCEP GRIB2 Table 4.2, where also, temperature = 0).

        jpdt(1) = 0  ! Parameter category (temperature = 0)
        jpdt(2) = 0  ! Parameter number (temperature = 0)

        jpdt(9) = ifcsthour

        call getgb2(lugb,lugi,jskp,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt
     &             ,unpack,krec,holdgfld,iret)
        if ( iret.ne.0) then
          print *,' '
          print *,' ERROR: getgb2 error in getgridinfo = ',iret
        endif

c       Determine packing information from GRIB2 file
c       The default packing is 40  JPEG 2000

        ipack = 40

        print *,' holdgfld%idrtnum = ', holdgfld%idrtnum

        !   Set DRT info  ( packing info )
        if ( holdgfld%idrtnum.eq.0 ) then      ! Simple packing
          ipack = 0
        elseif ( holdgfld%idrtnum.eq.2 ) then  ! Complex packing
          ipack = 2
        elseif ( holdgfld%idrtnum.eq.3 ) then  ! Complex & spatial
                                               ! packing
          ipack = 31
        elseif ( holdgfld%idrtnum.eq.40.or.holdgfld%idrtnum.eq.15 ) then
          ! JPEG 2000 packing
          ipack = 40
        elseif ( holdgfld%idrtnum.eq.41 ) then  ! PNG packing
          ipack = 41
        endif

        print *,'After check of idrtnum, ipack= ',ipack

        print *,'Number of gridpts= holdgfld%ngrdpts= ',holdgfld%ngrdpts
        print *,'Number of elements= holdgfld%igdtlen= '
     &         ,holdgfld%igdtlen
        print *,'PDT num= holdgfld%ipdtnum= ',holdgfld%ipdtnum
        print *,'GDT num= holdgfld%igdtnum= ',holdgfld%igdtnum

        imax = holdgfld%igdtmpl(8)
        jmax = holdgfld%igdtmpl(9)
        dx   = float(holdgfld%igdtmpl(17))/1.e6
        dy   = float(holdgfld%igdtmpl(17))/1.e6
        kf   = holdgfld%ngrdpts

c        holdgfld = gfld
c
c        call gf_free (holdgfld)
    
      else

        ! Search for a record from a GRIB1 file

        jpds = -1
        jgds = -1

        j=0

        jpds(5)  = iparm  ! Get a temperature record
        jpds(6)  = 100    ! Get a record on a standard pressure level
        jpds(14) = ifcsthour
         
        call getgb(lugb,lugi,jf,j,jpds,jgds,
     &                       kf,k,kpds,kgds,lb,f,iret)

        if (iret.ne.0) then
          print *,' '
          print *,'!!! ERROR in tave  getgridinfo calling getgb'
          print *,'!!! Return code from getgb = iret = ',iret
          iggret = iret
          return
        else
          iggret=0
          imax = kgds(2)
          jmax = kgds(3)
          dx   = float(kgds(9))/1000.
          dy   = float(kgds(10))/1000.
        endif

      endif

      print *,' '
      print *,'In getgridinfo, grid dimensions follow:'
      print *,'imax= ',imax,' jmax= ',jmax
      print *,'  dx= ',dx,'  dy= ',dy
      print *,'number of gridpoints = ',kf
      
      deallocate (lb); deallocate(f)
      
      return
      end

c---------------------------------------------------------------------
c     
c---------------------------------------------------------------------
      subroutine getgridinfo_netcdf (ncfile_id,imax,jmax,kf,netcdfinfo
     &                              ,iggret)
c     
c     ABSTRACT: The purpose of this subroutine is just to get the max
c     values of i and j and the dx and dy grid spacing intervals for the
c     grid to be used in the rest of the program.  So just query the
c     netcdf file to get the lon and lat data.  Also, get the info for
c     the data grid's boundaries.  This boundary information will be 
c     used later in the tracking algorithm, and is accessed via Module
c     grid_bounds.
c     
      USE netcdf_parms

      implicit none
c     
      type (netcdfstuff) netcdfinfo

      real      xhold,xlondiff,xlatdiff
      real, allocatable :: tmplon(:),tmplat(:)
      real(kind=4), allocatable :: temp_tmplon4(:),temp_tmplat4(:)
      real(kind=8), allocatable :: temp_tmplon8(:),temp_tmplat8(:)
      real, intent(out) :: dx,dy
      integer, intent(in)  :: ncfile_id
      integer, intent(out) :: imax,jmax,kf
      integer :: iggret
      integer :: iia,ija,midi,midj,i,j,iix,jix,xtype,ignrret
      integer :: ii4a,ii8a,ij4a,ij8a
c
      iggret = 0

      call get_ncdim1(ncfile_id,netcdfinfo%lon_name,imax)
      call get_ncdim1(ncfile_id,netcdfinfo%lat_name,jmax)

      if (allocated(tmplon)) deallocate (tmplon)
      if (allocated(tmplat)) deallocate (tmplat)
      allocate (tmplon(imax),stat=iia)
      allocate (tmplat(jmax),stat=ija)
      if (iia /= 0 .or. ija /= 0) then
        print *,' '
        print *,'!!! ERROR in sub getgridinfo_netcdf allocating arrays.'
        print *,'!!! iia = ',iia,' ija= ',ija
        iggret = 94
        return
      endif

      if (allocated(temp_tmplon4)) deallocate (temp_tmplon4)
      if (allocated(temp_tmplon8)) deallocate (temp_tmplon8)
      if (allocated(temp_tmplat4)) deallocate (temp_tmplat4)
      if (allocated(temp_tmplat8)) deallocate (temp_tmplat8)
      allocate (temp_tmplon4(imax),stat=ii4a)
      allocate (temp_tmplon8(imax),stat=ii8a)
      allocate (temp_tmplat4(jmax),stat=ij4a)
      allocate (temp_tmplat8(jmax),stat=ij8a)
      if (ii4a /= 0 .or. ii8a /= 0 .or.
     &    ij4a /= 0 .or. ij8a /= 0) then
        print *,' '
        print *,'!!! ERROR in sub getgridinfo_netcdf allocating'
        print *,'!!! temp_tmplon or temp_tmplat arrays.'
        print *,'!!! ii4a = ',ii4a,' ii8a= ',ii8a
        print *,'!!! ij4a = ',ij4a,' ij8a= ',ij8a
        iggret = 94
        return
      endif

      if (verb .ge. 1) then
        print *,'in getgridinfo_netcdf, ncfile_id= ',ncfile_id
      endif

      ! Get type (32/64-bit) of longitude, then read longitude into
      ! correctly sized array

      call get_netcdf_real_type (ncfile_id,netcdfinfo%lon_name
     &                                  ,xtype,ignrret)

      if (xtype == 5) then
        call get_var1_one_dim4 (ncfile_id,netcdfinfo%lon_name,imax
     &                        ,temp_tmplon4)
        tmplon = temp_tmplon4
      else
        call get_var1_one_dim8 (ncfile_id,netcdfinfo%lon_name,imax
     &                        ,temp_tmplon8)
        tmplon = temp_tmplon8
      endif

      if (allocated(temp_tmplon4)) deallocate (temp_tmplon4)
      if (allocated(temp_tmplon8)) deallocate (temp_tmplon8)

      ! Get type (32/64-bit) of latitude, then read latitude into
      ! correctly sized array

      call get_netcdf_real_type (ncfile_id,netcdfinfo%lat_name
     &                                  ,xtype,ignrret)

      if (xtype == 5) then
        call get_var1_one_dim4 (ncfile_id,netcdfinfo%lat_name,jmax
     &                        ,temp_tmplat4)
        tmplat = temp_tmplat4
      else
        call get_var1_one_dim8 (ncfile_id,netcdfinfo%lat_name,jmax
     &                        ,temp_tmplat8)
        tmplat = temp_tmplat8
      endif

      if (allocated(temp_tmplat4)) deallocate (temp_tmplat4)
      if (allocated(temp_tmplat8)) deallocate (temp_tmplat8)


c     Compute the dx and dy by picking values out of the middle of
c     the lat and lon arrays....

      midi = imax/2
      midj = jmax/2

      dx = abs(tmplon(midi) - tmplon(midi-1))
      dy = abs(tmplat(midj) - tmplat(midj-1))
      kf = imax * jmax

      if (verb .ge. 1) then
        print *,' '
        print *,'In getgridinfo_netcdf, grid dimensions follow:'
        print *,'imax= ',imax,' jmax= ',jmax
        print *,'dx=   ',dx,' dy= ',dy
        print *,'kf=   ',kf
        print *,' '
        write (6,112) midi,dx
        write (6,113) midj,dy
        write (6,114) kf

 112    format(1x,' DX:  midi= ',i4,' dx= ',f8.4)
 113    format(1x,' DY:  midj= ',i4,' dy= ',f8.4)
 114    format(1x,' kf:  kf= ',i6)
      endif
c
      return
      end
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine getdata_grib (lugb,lugi,kf,valid_pt,nlevsin,ilevs
     &             ,readflag,xinptmp,ifcsthour,iparm,gribver
     &             ,g2_jpdtn,igdret)
c
c     ABSTRACT: This subroutine reads the input GRIB file for the
c     tracked parameters.

      USE params
      USE grib_mod

      implicit none
c
      type(gribfield) :: gfld
      CHARACTER(len=8) :: ctemp,pabbrev
      CHARACTER(len=80) :: ftemplate
      integer,dimension(200) :: jids,jpdt,jgdt
      integer, parameter :: jf=40000000
      integer   ilevs(nlevsin)
      integer   jpds(200),jgds(200),kpds(200),kgds(200)
      integer   lugb,lugi,kf,nlevsin,igdret,iparm,jskp,jdisc
      integer   jpdtn,jgdtn,npoints,icount,ipack,krec
      integer   i,j,k,ict,np,lev,ifcsthour,iret,gribver,g2_jpdtn
      integer   pdt_4p0_vert_level,pdt_4p0_vtime,mm
      integer :: listsec0(2)=(/0,2/)
      integer :: listsec1(13)
      integer :: igds(5)=(/0,0,0,0,0/),previgds(5)
      integer :: idrstmpl(200)
      integer :: currlen=1000000
      logical :: unpack=.true.
      logical :: open_grb=.false.
      logical(1)  valid_pt(kf),lb(kf),readflag(nlevsin)
      real      f(kf),xinptmp(kf,nlevsin),xtemp(kf)
      real      dmin,dmax,firstval,lastval
c
      igdret=0
      ict = 0

      print *,'At top of getdata, ifcsthour= ',ifcsthour

      level_loop: do lev = 1,nlevsin

        print *,' '
        print *,'------------------------------------------------'
        print *,'In tave  getdata read loop, lev= ',lev,' level= '
     &         ,ilevs(lev)

        if (gribver == 2) then

          !
          ! ---  Initialize Variables ---
          !

          gfld%idsect => NULL()
          gfld%local => NULL()
          gfld%list_opt => NULL()
          gfld%igdtmpl => NULL()
          gfld%ipdtmpl => NULL()
          gfld%coord_list => NULL()
          gfld%idrtmpl => NULL()
          gfld%bmap => NULL()
          gfld%fld => NULL()

          jdisc=0  ! Meteorological products
          jids=-9999
          jpdtn=g2_jpdtn ! 0 = analysis or forecast; 1 = ens fcst
          jgdtn=0  ! lat/lon grid
          jgdt=-9999
          jpdt=-9999

          npoints=0
          icount=0
          jskp=0

c         Search for input parameter by production template 4.0.  This
c         tave program is used primarily for temperature, but still we
c         will leave that as a variable and not-hard wire it in case we
c         choose to average something else in the future.

          if (iparm == 11) then

            ! Set defaults for JPDT, then override in array 
            ! assignments below...

            JPDT(1:15)=(/ -9999,-9999,-9999,-9999,-9999,-9999,-9999
     &             ,-9999,-9999,-9999,-9999,-9999,-9999,-9999,-9999/)
            JPDT(1)  = 0   ! Param category from Table 4.1
            JPDT(2)  = 0   ! Param number from Table 4.2
            JPDT(9)  = ifcsthour
            JPDT(10) = 100 ! Isobaric surface requested (Table 4.5)
            JPDT(12) = ilevs(lev) * 100 ! value of specific level

            print *,'In getdata, just set JPDT inputs....'

          endif

          print *,'before getgb2 call, value of unpack = ',unpack
 
          do mm = 1,15
            print *,'tave  getdata mm= ',mm,' JPDT(mm)= ',JPDT(mm)
          enddo

          call getgb2(lugb,lugi,jskp,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt
     &             ,unpack,krec,gfld,iret)

          print *,'iret from getgb2 in getdata = ',iret

          print *,'after getgb2 call, value of unpacked = '
     &           ,gfld%unpacked

          print *,'after getgb2 call, gfld%ngrdpts = ',gfld%ngrdpts
          print *,'after getgb2 call, gfld%ibmap = ',gfld%ibmap

          if ( iret == 0) then

c           Determine packing information from GRIB2 file
c           The default packing is 40  JPEG 2000

            ipack = 40

            print *,' gfld%idrtnum = ', gfld%idrtnum

            !   Set DRT info  ( packing info )
            if ( gfld%idrtnum.eq.0 ) then      ! Simple packing
              ipack = 0
            elseif ( gfld%idrtnum.eq.2 ) then  ! Complex packing
              ipack = 2
            elseif ( gfld%idrtnum.eq.3 ) then  ! Complex & spatial 
     &                                         ! packing
              ipack = 31
            elseif ( gfld%idrtnum.eq.40.or.gfld%idrtnum.eq.15 ) then
              ! JPEG 2000 packing
              ipack = 40
            elseif ( gfld%idrtnum.eq.41 ) then  ! PNG packing
              ipack = 41
            endif

            print *,'After check of idrtnum, ipack= ',ipack

            print *,'Number of gridpts= gfld%ngrdpts= ',gfld%ngrdpts
            print *,'Number of elements= gfld%igdtlen= ',gfld%igdtlen
            print *,'GDT num= gfld%igdtnum= ',gfld%igdtnum

            kf = gfld%ngrdpts  ! Number of gridpoints returned from read

            do np = 1,kf
              xinptmp(np,lev)  = gfld%fld(np)
              xtemp(np)        = gfld%fld(np)
              if (gfld%ibmap == 0) then
                valid_pt(np)     = gfld%bmap(np)
              else
                valid_pt(np)     = .true.
              endif
            enddo

            readflag(lev) = .TRUE.
c            call bitmapchk(kf,gfld%bmap,gfld%fld,dmin,dmax)
            call bitmapchk(kf,valid_pt,xtemp,dmin,dmax)

            if (ict == 0) then
c              do np = 1,kf
c                valid_pt(np) = gfld%bmap(np)
c              enddo
              ict = ict + 1
            endif

            firstval=gfld%fld(1)
            lastval=gfld%fld(kf)

            print *,' '
            print *,' SECTION 0: discipl= ',gfld%discipline
     &             ,' gribver= ',gfld%version

            print *,' '
            print *,' SECTION 1: '

            do j = 1,gfld%idsectlen
              print *,'     sect1, j= ',j,' gfld%idsect(j)= '
     &               ,gfld%idsect(j)
            enddo

            if ( associated(gfld%local).AND.gfld%locallen.gt.0) then
              print *,' '
              print *,' SECTION 2: ',gfld%locallen,' bytes'
            else 
              print *,' '
              print *,' SECTION 2 DOES NOT EXIST IN THIS RECORD'
            endif

            print *,' '
            print *,' SECTION 3: griddef= ',gfld%griddef
            print *,'            ngrdpts= ',gfld%ngrdpts
            print *,'            numoct_opt= ',gfld%numoct_opt
            print *,'            interp_opt= ',gfld%interp_opt
            print *,'            igdtnum= ',gfld%igdtnum
            print *,'            igdtlen= ',gfld%igdtlen

            print *,' '
            print '(a17,i3,a2)',' GRID TEMPLATE 3.',gfld%igdtnum,': '
            do j=1,gfld%igdtlen
              print *,'    j= ',j,' gfld%igdtmpl(j)= ',gfld%igdtmpl(j)
            enddo

            print *,' '
            print *,'     PDT num (gfld%ipdtnum) = ',gfld%ipdtnum
            print *,' '
            print '(a20,i3,a2)',' PRODUCT TEMPLATE 4.',gfld%ipdtnum,': '
            do j=1,gfld%ipdtlen
              print *,'    sect 4  j= ',j,' gfld%ipdtmpl(j)= '
     &               ,gfld%ipdtmpl(j)
            enddo 

c           Print out values for data representation type

            print *,' '
            print '(a21,i3,a2)',' DATA REP TEMPLATE 5.',gfld%idrtnum
     &            ,': '
            do j=1,gfld%idrtlen
              print *,'    sect 5  j= ',j,' gfld%idrtmpl(j)= '
     &               ,gfld%idrtmpl(j)
            enddo

            pdt_4p0_vtime      = gfld%ipdtmpl(9)
            pdt_4p0_vert_level = gfld%ipdtmpl(12)

c           Get parameter abbrev for record that was retrieved

            pabbrev=param_get_abbrev(gfld%discipline,gfld%ipdtmpl(1)
     &                              ,gfld%ipdtmpl(2))

            print *,' '
            write (6,131)
 131        format (' rec#   param     level  byy  bmm  bdd  bhh  '
     &             ,'fhr      npts  firstval    lastval     minval   '
     &             ,'   maxval')
            print '(i5,3x,a8,2x,6i5,2x,i8,4g12.4)'
     &          ,krec,pabbrev,pdt_4p0_vert_level/100,gfld%idsect(6)
     &             ,gfld%idsect(7),gfld%idsect(8),gfld%idsect(9)
     &             ,pdt_4p0_vtime,gfld%ngrdpts,firstval,lastval
     &             ,dmin,dmax

c            do np = 1,kf
c              xinptmp(np,lev) = gfld%fld(np)
c            enddo

          else

            print *,' '
            print *,'!!! ERROR: GRIB2 TAVE READ IN GETDATA FAILED FOR '
     &             ,'LEVEL LEV= ',LEV
            print *,' '

            readflag(lev) = .FALSE.

            do np = 1,kf
              xinptmp(np,lev) = -99999.0
            enddo

          endif

          call gf_free (gfld)

        else

          ! Reading a GRIB1 file....

          jpds = -1
          jgds = -1
          j=0

          jpds(5) = iparm       ! parameter id for temperature
          jpds(6) = 100         ! level id to indicate a pressure level
          jpds(7) = ilevs(lev)  ! actual level of the layer
          jpds(14) = ifcsthour  ! lead time to search for

          call getgb (lugb,lugi,jf,j,jpds,jgds,
     &                          kf,k,kpds,kgds,lb,f,iret)

          print *,' '
          print *,'After tave getgb call, j= ',j,' k= ',k,' level= '
     &           ,ilevs(lev),' iret= ',iret

          if (iret == 0) then

            readflag(lev) = .TRUE.
            call bitmapchk(kf,lb,f,dmin,dmax)

            if (ict == 0) then
              do np = 1,kf
                valid_pt(np) = lb(np)
              enddo
              ict = ict + 1
            endif

            write (6,31)
  31        format (' rec#  parm# levt lev  byy   bmm  bdd  bhh  fhr  '
     &             ,'npts  minval       maxval')
            print '(i4,2x,8i5,i8,2g12.4)',
     &           k,(kpds(i),i=5,11),kpds(14),kf,dmin,dmax

            do np = 1,kf
              xinptmp(np,lev) = f(np) 
            enddo

          else
  
            print *,' '
            print *,'!!! ERROR: TAVE READ FAILED FOR LEVEL LEV= ',LEV
            print *,' '

            readflag(lev) = .FALSE.

            do np = 1,kf
              xinptmp(np,lev) = -99999.0
            enddo

          endif

        endif

      enddo level_loop
c
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine getdata_netcdf (ncfile_id,imax,jmax,kf,readflag,xinptmp
     &               ,ifcsthour,ifh,ncfile_tmax,netcdfinfo,nlevsin
     &               ,cnvname_300,cnvname_350,cnvname_400
     &               ,cnvname_450,cnvname_500,valid_pt,ignret)
c
c     ABSTRACT: This subroutine reads the input NetCDF file for the 
c     temperature data for one lead time.  There are 5 records for 
c     temperature that will be read in, one for each level of 300, 
c     350, 400, 450 and 500 mb.
c
c     INPUT:
c     ncfile_id   integer ID associated with the NetCDF file
c     imax        integer number of points in i-direction on grid
c     jmax        integer number of points in j-direction on grid
c     kf          integer number of points (imax*jmax) on grid
c     ifcsthour   integer lead time (forecast hour)
c     ifh         integer index for forecast hour
c     ncfile_tmax integer with max number of time levels in the input
c                 NetCDF file, as read in from the NetCDF file
c                 itself in subroutine  read_netcdf_fhours.
c     netcdfinfo  variable of user-defined type netcdfstuff (from 
c                 module netcdf_parms).
c     nlevsin     integer number of vertical levels to be read in.
c     cnvname_300 character name of 300 mb variable
c     cnvname_350 character name of 350 mb variable
c     cnvname_400 character name of 400 mb variable
c     cnvname_450 character name of 450 mb variable
c     cnvname_500 character name of 500 mb variable
c
c     OUTPUT:
c     readflag    logical array, indicates if a parm was read in
c     xinptmp     real array of data values
c     valid_pt    logical array, indicates for each (i,j) if there is
c                 valid data at the point (used for regional grids)
c     ignret      integer retur code from this subroutine

      USE tracked_parms; USE level_parms; USE inparms; USE phase
      USE netcdf_parms; USE verbose_output; USE read_parms
      USE genesis_diags; USE trkrparms; USE vortex_tilt_diags
      USE sst_diags

      implicit none
c
      type (trackstuff) trkrinfo
      type (netcdfstuff) netcdfinfo
      real, allocatable :: f(:)
      real(kind=4) :: f4(kf)
      real(kind=8) :: f8(kf)
      real :: dmin,dmax,xfill_value
      real :: xinptmp(kf,nlevsin)
      real         :: xmissing_value
      real(kind=4) :: xmissing_val4
      real(kind=8) :: xmissing_val8
      logical(1) valid_pt(kf),readflag(nreadparms)
      logical(1) readgenflag(nreadgenparms)
      logical(1) ::  need_to_flip_lats,need_to_flip_lons
      character*1  :: lbrdflag,match_check,match_zero_check
      character*40, allocatable :: cnc_tilt_var(:,:)
      character*40 :: cvar
      character*60 :: cnvname_300,cnvname_350,cnvname_400
      character*60 :: cnvname_450,cnvname_500
      character*60 :: chparm(nreadparms)
      integer, intent(in) :: ncfile_id,nc_lsmask_file_id,imax,jmax,kf
      integer, parameter :: nreadparms=5
      integer :: igvret,ifa,ip,ifh,i,j,k,m,n,ncfile_tmax,nf_get_att_real
      integer :: nf_get_att_double,nf_inq_attlen,imvlen,ifvlen
      integer :: usertime,ncix,missing_val_length,nf_status
      integer :: nf_inq_varid,varid,igrh,igrhct,nc_zero_ix,np,ict
      integer :: xtype,ignrret,ilev,ilevix,ictvret,ictvpret,ignret
      integer :: num_vortex_tilt_levs,iprslev,ilevct,ilevmod,nlevsin
c
      lbrdflag = 'n'

c     Load the names of the NetCDF variables into the chparm array...

      chparm(1)  = cnvname_300
      chparm(2)  = cnvname_350
      chparm(3)  = cnvname_400
      chparm(4)  = cnvname_450
      chparm(5)  = cnvname_500
      
      if (verb .ge. 3) then
        print *,' '
        print *,'NOTE: Program is now in subroutine  getdata_netcdf.'
      endif

      if (allocated(f)) deallocate(f)
      allocate (f(kf),stat=ifa)
      if (ifa /= 0) then
        print *,' '
        print *,'!!! ERROR in getdata_netcdf allocating f data array.'
        print *,'!!! ifa = ',ifa
        print *,'!!! STOPPING EXECUTION'
        STOP 91
      endif

      !---------------------------------------------------------------
      ! First go through the list of user-requested lead times that 
      ! were read in from subroutine  read_fhours and try to match up
      ! the lead times that were read in with the lead times that 
      ! we read in directly from the NetCDF file.  Get the index from
      ! the NetCDF file for that lead time and use that in the call to
      ! the read routine (get_var3_tlev_double).
      !---------------------------------------------------------------

      usertime = ifcsthour

      match_check = 'n'

      find_index_loop: do m = 1,ncfile_tmax

        if (usertime == nctotalhours(m)) then
          ncix = m
          if (verb .ge. 1) then
            print *,'+++ Time match in getdata_netcdf for usertime= '
     &             ,usertime,'  netcdf file index= ncix= ',ncix
          endif
          match_check = 'y'
          exit find_index_loop
        endif

      enddo find_index_loop

      if (match_check == 'n') then
        print *,' '
        print *,'!!! ERROR in getdata_netcdf: '
        print *,'  For a NetCDF file, the user has '
        print *,'  requested to process a lead time, and that lead'
        print *,'  time does not exist in the NetCDF list of time'
        print *,'  values. '
        print *,'  ifh= ',ifh
        print *,'  usertime= ifcsthour= ',ifcsthour
        print *,'  STOPPING....'
        stop 99
      endif

      !---------------------------------------------------------------
      ! NetCDF read for variables on the 5 standard levels.
      !
      ! Now go through the read loop for the list of parameters
      !
      ! This is the NetCDF reading section.
      !---------------------------------------------------------------

c      netcdf_standard_parm_read_loop: do ip = 1,nreadparms

      level_loop: do lev = 1,nlevsin

        if (chparm(lev) == 'X' .or. chparm(lev) == 'x') then
          if (verb .ge. 3) then
            print *,' '
            print *,'!!! ERROR: NetCDF read NOT requested for level # '
     &             ,lev
            print *,'!!! For this tave program, that is unusual since'
            print *,'!!! we have 5 specific vertical levels to read in.'
            stop 99
          endif
          cycle level_loop
        else
          if (verb .ge. 3) then
            print *,' '
            print '(a37,i2,a11,a15)'
     &           ,'+++ NetCDF read requested for level # ',lev
     &             ,' ... parm= ',chparm(lev)
          endif
        endif

        call get_netcdf_real_type (ncfile_id,chparm(lev)
     &                            ,xtype,ignrret)
        if (xtype == 5) then
          call get_var3_tlev_real4 (ncfile_id,chparm(lev)
     &                             ,imax,jmax,ncix,f4,igvret)
          f = f4
        else
          call get_var3_tlev_double (ncfile_id,chparm(lev)
     &                      ,imax,jmax,ncix,f8,igvret)
          f = f8
        endif
        if (verb .ge. 3) then
          print *,'After read, parm= ',chparm(lev),' ifh= ',ifh
     &           ,' lead time index= ',ltix(ifh),' parm# (lev) = ',lev
     &           ,' ncix= ',ncix,' igvret= ',igvret
        endif

        if (igvret == 0) then

c          call bitmapchk(kf,lb,f,dmin,dmax)

          readflag(lev) = .TRUE.
          dmin = minval(f)
          dmax = maxval(f)

          ! Need to get the value of the "missing_value" attribute for
          ! this variable from the list of attributes in the NetCDF
          ! file.  Only do this for the first lead time, since the 
          ! value of the "missing_value" obviously will not change 
          ! with lead time.

c          nf_status = nf_inq_attlen (ncfile_id,varid,"missing_value"
c     &               ,imvlen)
c          nf_status = nf_inq_attlen (ncfile_id,varid,"_FillValue"
c     &               ,ifvlen)

          ! These next two nf function calls retrieve the value of the
          ! "missing_value" attribute from the list of attributes for 
          ! the given variable being read in.  This is needed in order
          ! to know if a non-valid point is being accessed, as for a 
          ! regional grid, like the nested fvGFS.  In GRIB1/GRIB2 files,
          ! such regions would be bitmapped out, but in a NetCDF file,
          ! no such bitmap exists, so we have to check for missing 
          ! values.  In case it's a moving grid, we need to do this 
          ! for every lead time, since the "map of missing values" 
          ! will shift with lead time.  Once we have those missing
          ! values, we can loop through them and fill the valid_pt
          ! logical array so that, in the end, we will have the same
          ! logical bitmap for masking out missing data that we have
          ! with GRIB1/GRIB2 data.

          nf_status = nf_inq_varid (ncfile_id,chparm(lev),varid)

          print *,'nf_status from nf_inq_varid call = ',nf_status

          call get_netcdf_real_type (ncfile_id,chparm(lev)
     &                              ,xtype,ignrret)

          if (xtype == 5) then
            nf_status = nf_get_att_real (ncfile_id,varid
     &                 ,"missing_value",xmissing_val4)
            xmissing_value = xmissing_val4
          else
            nf_status = nf_get_att_double (ncfile_id,varid
     &                 ,"missing_value",xmissing_val8)
            xmissing_value = xmissing_val8
          endif

          print *,'nf_status from nf_get_att_real call = ',nf_status

c          nf_status = nf_get_att_real (ncfile_id,varid,"_FillValue"
c     &               ,xfill_value)
c          nf_status = nf90_inquire_attribute (ncfile_id,chparm(lev)
c     &               ,"missing_value",len=imvlen)
c          nf_status = nf90_inquire_attribute (ncfile_id,chparm(lev)
c     &               ,"_FillValue",len=ifvlen)
c
c          nf_status = nf90_get_att (ncfile_id,chparm(lev)
c     &               ,"missing_value",xmissing_value)
c          nf_status = nf90_get_att (ncfile_id,chparm(lev)
c     &               ,"_FillValue",xfill_value)

          if (verb .ge. 3) then
            write (6,31)
  31        format ('parmread lead time     parm#        parm_id   '
     &             ,23x,'minval       maxval')

            write (6,33) ifhours(ifh),ifclockmins(ifh),lev,chparm(lev)
     &                ,dmin,dmax
  33        format ('   ',i3,':',i2.2,14x,i3,10x,a30,1x,2g12.4)
            write (6,35) chparm(lev),xmissing_value
  35        format ('   --- ',a30,' missing value = ',g12.4)
          endif

          ! This call to create_bitmap_netcdf creates
          ! a logical bitmap, so that in case we have 
          ! regional (non-global) data and an irregular grid (e.g., 
          ! the FV3 nested grid), we can mask out grid points that 
          ! have missing values as their data values.  There is not
          ! actually a native logical bitmap in NetCDF, so we will
          ! create one by examining the real data values and masking
          ! out grid points that have missing values.

          if (lbrdflag .eq. 'n') then
            call create_bitmap_netcdf (imax,jmax,f,valid_pt
     &                    ,xmissing_value)
            lbrdflag = 'y'
          endif

          do np = 1,kf
            xinptmp(np,lev) = f(np) 
          enddo

stopped here.  Just created the bitmap and put the data into the 
xinptmp data array.  need to get rid of the rest
of this stuff just below here that converts to 2d arrays.  don't need it.
          

          if (ip == 1) then         ! 850 mb absolute vorticity
            call conv1d2d_real_netcdf (imax,jmax,f,zeta(1,1,1)
     &                                     ,need_to_flip_lats)
          else if (ip == 2) then    ! 700 mb absolute vorticity
            call conv1d2d_real_netcdf (imax,jmax,f,zeta(1,1,2)
     &                                     ,need_to_flip_lats)
          else if (ip == 3) then    ! 850 mb u-comp
            call conv1d2d_real_netcdf (imax,jmax,f,u(1,1,nlev850)
     &                                     ,need_to_flip_lats)
          else if (ip == 4) then    ! 850 mb v-comp
            call conv1d2d_real_netcdf (imax,jmax,f,v(1,1,nlev850)
     &                                     ,need_to_flip_lats)
          else if (ip == 5) then    ! 700 mb u-comp
            call conv1d2d_real_netcdf (imax,jmax,f,u(1,1,nlev700)
     &                                     ,need_to_flip_lats)
          else if (ip == 6) then    ! 700 mb v-comp
            call conv1d2d_real_netcdf (imax,jmax,f,v(1,1,nlev700)
     &                                     ,need_to_flip_lats)
          else if (ip == 7) then    ! 850 mb gp height
            call conv1d2d_real_netcdf (imax,jmax,f,hgt(1,1,1)
     &                                     ,need_to_flip_lats)
          else if (ip == 8) then    ! 700 mb gp height
            call conv1d2d_real_netcdf (imax,jmax,f,hgt(1,1,2)
     &                                     ,need_to_flip_lats)
          else if (ip == 9) then    ! MSLP
            call conv1d2d_real_netcdf (imax,jmax,f,slp
     &                                     ,need_to_flip_lats)
          else if (ip == 10) then   ! Near-sfc (10m) u-comp
            call conv1d2d_real_netcdf (imax,jmax,f,u(1,1,levsfc)
     &                                     ,need_to_flip_lats)
          else if (ip == 11) then   ! Near-sfc (10m) v-comp
            call conv1d2d_real_netcdf (imax,jmax,f,v(1,1,levsfc)
     &                                     ,need_to_flip_lats)
          else if (ip == 12) then   ! 500 mb u-comp
            call conv1d2d_real_netcdf (imax,jmax,f,u(1,1,nlev500)
     &                                     ,need_to_flip_lats)
          else if (ip == 13) then   ! 500 mb v-comp
            call conv1d2d_real_netcdf (imax,jmax,f,v(1,1,nlev500)
     &                                     ,need_to_flip_lats)
          else if (ip == 14) then   ! 300-500 mb mean Temp
            call conv1d2d_real_netcdf (imax,jmax,f,tmean
     &                                     ,need_to_flip_lats)
          else if (ip == 15) then   ! 500 mb height
            call conv1d2d_real_netcdf (imax,jmax,f,hgt(1,1,3)
     &                                     ,need_to_flip_lats)
          else if (ip == 16) then   ! 200 mb height
            call conv1d2d_real_netcdf (imax,jmax,f,hgt(1,1,4)
     &                                     ,need_to_flip_lats)
          else if (ip == 17) then   ! Land-sea mask
            call conv1d2d_real_netcdf (imax,jmax,f,lsmask
     &                                     ,need_to_flip_lats)
          else if (ip == 18) then   ! 200 mb u-comp
            call conv1d2d_real_netcdf (imax,jmax,f,u(1,1,nlev200)
     &                                     ,need_to_flip_lats)
          else if (ip == 19) then   ! 200 mb v-comp
            call conv1d2d_real_netcdf (imax,jmax,f,v(1,1,nlev200)
     &                                     ,need_to_flip_lats)
          else if (ip == 20) then   ! SST
              call conv1d2d_real_netcdf (imax,jmax,f,sst(1,1)
     &                                   ,need_to_flip_lats)
          else

            print *,'!!! NOTE: Parm not recognized. '
            print *,'!!!       ip is > 20.... ip= ',ip
            print *,'!!!       Forecast time level = ',ifh

          endif

        endif

      enddo netcdf_standard_parm_read_loop

c     *--------------------------------------------------------------*
c      NetCDF read for Cyclone Phase Space diagnostics
c
c      If we are attempting to determine the cyclone structure using
c      Hart's cyclone phase space, then read in data now that will 
c      allow us to do that.  If we are instead just using the 
c      mid-level (300-500 mb) mean temperature to do that with a 
c      simple warm-core check, then that mean temperature field was
c      already read in above in the read loop for the standard 
c      variables.  The variables needed here for CPS are pretty 
c      straightforward:  gp height every 50 mb from 300 to 900 mb.
c      keep in mind that we have already read in a few of these 
c      gp height records for selected levels above.
c     *--------------------------------------------------------------*

      if (phaseflag == 'y') then

        if (phasescheme == 'cps' .or. phasescheme == 'both') then

          chparm_cps(1)  = netcdfinfo%z900name
          chparm_cps(2)  = netcdfinfo%z850name
          chparm_cps(3)  = netcdfinfo%z800name
          chparm_cps(4)  = netcdfinfo%z750name
          chparm_cps(5)  = netcdfinfo%z700name
          chparm_cps(6)  = netcdfinfo%z650name
          chparm_cps(7)  = netcdfinfo%z600name
          chparm_cps(8)  = netcdfinfo%z550name
          chparm_cps(9)  = netcdfinfo%z500name
          chparm_cps(10) = netcdfinfo%z450name
          chparm_cps(11) = netcdfinfo%z400name
          chparm_cps(12) = netcdfinfo%z350name
          chparm_cps(13) = netcdfinfo%z300name

          ! Read in GP Height levels for cyclone phase space...

          if (verb .ge. 3) then
            print *,' '
            print *,'--- Reads for CPS parms follow...'
            print *,' '
          endif

          netcdf_cps_parm_read_loop: do ip = 1,nreadcpsparms

            if (chparm_cps(ip) == 'X' .or. chparm_cps(ip) == 'x') then
              if (verb .ge. 3) then
                print *,'!!! ERROR: NetCDF read NOT requested for'
                print *,'!!! CPS parm # ',ip
                print *,'!!! You must have an error in your namelist.'
                print *,'!!! You have requested to do cyclone phase'
                print *,'!!! checking, so you need to include the '
                print *,'!!! NetCDF names for ALL requested gp height'
                print *,'!!! variables from 900 to 300 mb, every 50 '
                print *,'!!! mb,in the namelist.'
                print *,'!!! phaseflag is being set to NO (n), and '
                print *,'!!! phase-checking will NOT take place.'
                print *,'!!! If you want to run again and just do '
                print *,'!!! phase-checking with a simple warm-core'
                print *,'!!! check, then in the namelist set phaseflag'
                print *,'!!! to y and set phasescheme to vtt.'
              endif
              phaseflag = 'n'
              exit netcdf_cps_parm_read_loop
            else
              if (verb .ge. 3) then
                print *,'+++ NetCDF read requested for cps parm # ',ip
     &                 ,' ... parm= ',chparm_cps(ip)
              endif
            endif

            ! As above, we send a 1-d array, "f", to the netcdf read
            ! routine.   While that routine returns a 2-d array (which 
            ! we want), depending on the model & grid, we may need to 
            ! flip the grid in the north-south direction.  I already 
            ! have a routine for converting data from a 1-d to a 2-d 
            ! array, and it has the functionality for flipping a grid, 
            ! so I programmed it as getting a 1-d array from the netcdf
            ! read routine and send that 1-d array to conv1d2d_real.

            call get_netcdf_real_type (ncfile_id,chparm_cps(ip)
     &                                ,xtype,ignrret)

            if (xtype == 5) then
              call get_var3_tlev_real4 (ncfile_id,chparm_cps(ip),imax
     &                    ,jmax,ncix,f4,igvret)
              f = f4
            else
              call get_var3_tlev_double (ncfile_id,chparm_cps(ip),imax
     &                    ,jmax,ncix,f8,igvret)
              f = f8
            endif

            if (verb .ge. 3) then
              print *,' '
              print *,'After read, parm= ',chparm_cps(ip),' ifh= ',ifh
     &               ,' lead time index= ',ltix(ifh),' parm# (ip) = ',ip
     &               ,' ncix= ',ncix,' igvret= ',igvret
            endif

            if (igvret == 0) then

c              call bitmapchk(kf,lb,f,dmin,dmax)

              dmin = minval(f)
              dmax = maxval(f)

c              nf_status = nf_get_att_double (ncfile_id,chparm(ip)
c     &                   ,"missing_value",xmissing_value)
c              nf_status = nf_get_att_double (ncfile_id,chparm(ip)
c     &                   ,"_FillValue",xfill_value)

              
              nf_status = nf_inq_varid (ncfile_id,chparm_cps(ip),varid)

              call get_netcdf_real_type (ncfile_id,chparm_cps(ip)
     &                                  ,xtype,ignrret)

              if (xtype == 5) then
                nf_status = nf_get_att_real (ncfile_id,varid
     &                      ,"missing_value",xmissing_val4)
                xmissing_value = xmissing_val4
              else
                nf_status = nf_get_att_double (ncfile_id,varid
     &                      ,"missing_value",xmissing_val8)
                xmissing_value = xmissing_val8
              endif

              if (verb .ge. 3) then
                write (6,231)
 231            format ('parmread lead time     parm#        parm_id   '
     &                 ,23x,'minval       maxval')

                write (6,233) ifhours(ifh),ifclockmins(ifh),ip
     &                       ,chparm_cps(ip),dmin,dmax
 233            format ('   ',i3,':',i2.2,14x,i3,10x,a30,1x,2g12.4)
                write (6,235) chparm_cps(ip),xmissing_value
 235            format ('   --- ',a30,' missing value = ',g12.4)
              endif

              call conv1d2d_real_netcdf (imax,jmax,f,cpshgt(1,1,ip)
     &                           ,need_to_flip_lats)
 
            endif
                 
          enddo netcdf_cps_parm_read_loop
      
        endif
      
      endif

c     *------------------------------------------------------------*
c      NetCDF Read for genesis diagnostics
c
c      If we are attempting to perform genesis diagnostics, then 
c      read in data now that will allow us to do that.
c
c      The order of the variables in the reads is set up so that, 
c      ideally, we will read in the first 9 fields and not need 
c      anything else, e.g., SST, q850, and then RH at these 
c      levels: 1000, 925, 800, 750, 700, 650, 600 mb.  However, 
c      some models, like SHiELD & T-SHiELD, do not have RH at these
c      levels, but they do have T & q, so in those cases we would 
c      have to compute RH, and therefore need to read in T & q at
c      those levels.
c     *------------------------------------------------------------*

      if (genflag == 'y') then

        chparm_gen(1)  = netcdfinfo%q850name
        chparm_gen(2)  = netcdfinfo%t850name
        chparm_gen(3)  = netcdfinfo%rh850name
        chparm_gen(4)  = netcdfinfo%rh1000name
        chparm_gen(5)  = netcdfinfo%rh925name
        chparm_gen(6)  = netcdfinfo%rh800name
        chparm_gen(7)  = netcdfinfo%rh750name
        chparm_gen(8)  = netcdfinfo%rh700name
        chparm_gen(9)  = netcdfinfo%rh650name
        chparm_gen(10)  = netcdfinfo%rh600name
        chparm_gen(11)  = netcdfinfo%spfh1000name
        chparm_gen(12) = netcdfinfo%spfh925name
        chparm_gen(13) = netcdfinfo%spfh800name
        chparm_gen(14) = netcdfinfo%spfh750name
        chparm_gen(15) = netcdfinfo%spfh700name
        chparm_gen(16) = netcdfinfo%spfh650name
        chparm_gen(17) = netcdfinfo%spfh600name
        chparm_gen(18) = netcdfinfo%temp1000name
        chparm_gen(19) = netcdfinfo%temp925name
        chparm_gen(20) = netcdfinfo%temp800name
        chparm_gen(21) = netcdfinfo%temp750name
        chparm_gen(22) = netcdfinfo%temp700name
        chparm_gen(23) = netcdfinfo%temp650name
        chparm_gen(24) = netcdfinfo%temp600name
        chparm_gen(25) = netcdfinfo%omega500name

        netcdf_gen_parm_loop: do ip = 1,nreadgenparms

          if (gen_read_rh_fields == 'y' ) then

            if (ip == 11) then

              ! The ip index is now at the point where we are past all
              ! of the reads for the different levels of RH.
              ! Check the readgenflags for relative humidity.  If not
              ! enough RH records were read in, then we have to assume
              ! that RH was not included in the user data, so we will
              ! instead stay in this Genesis NetCDF read loop to read
              ! in q and T to compute RH later on.  If enough RH 
              ! records were read in, then exit this read loop.

              igrhct = 0
              do igrh = 4,10
                if (readgenflag(igrh)) then
                  igrhct = igrhct + 1
                endif
              enddo

              if (igrhct >= 2) then
                if (verb >= 3) then
                  print *,' '
                  print *,'Genesis NetCDF read: At least 2 RH records'
                  print *,'were read in, so we will exit the Genesis'
                  print *,'NetCDF read loop without reading specific'
                  print *,'humidity or temperature records.'
                endif
                need_to_compute_rh_from_q = 'n'
                exit netcdf_gen_parm_loop
              else
                if (verb >= 3) then
                  print *,' '
                  print *,'Genesis NetCDF read: Fewer than 2 RH records'
                  print *,'were read in, so we will continue in the'
                  print *,'Genesis NetCDF read loop, reading specific'
                  print *,'humidity and temperature records....'
                endif
                need_to_compute_rh_from_q = 'y'
              endif

            endif

          else

            need_to_compute_rh_from_q = 'y'

            ! If the ip index is between 4 and 10 (which is for RH
            ! records) and the user has specified that RH will NOT be
            ! read in, then skip over the read section for these by
            ! cycling.

            if (ip >= 4 .and. ip <= 10) then
              if (verb >= 3) then
                print *,' '
                print *,'Genesis read NOT requested for RH, ip= ',ip
                print *,' '
              endif
              cycle netcdf_gen_parm_loop
            endif

          endif

          if (chparm_gen(ip) == 'X' .or. chparm_gen(ip) == 'x') then
            if (verb .ge. 3) then
              print *,' '
              print *,'!!! NetCDF genesis read NOT requested for '
     &               ,'parm # ',ip
            endif
            cycle netcdf_gen_parm_loop
          else
            if (verb .ge. 3) then
              print *,' '
              print '(a45,i3,a11,a15)'
     &              ,'+++ NetCDF genesis read requested for parm # ',ip
     &              ,' ... parm= ',chparm_gen(ip)
            endif
          endif

          ! As above, we send a 1-d array, "f", to the netcdf read
          ! routine.   While that routine returns a 2-d array (which 
          ! we want), depending on the model & grid, we may need to 
          ! flip the grid in the north-south direction.  I already 
          ! have a routine for converting data from a 1-d to a 2-d 
          ! array, and it has the functionality for flipping a grid, 
          ! so I programmed it as getting a 1-d array from the netcdf
          ! read routine and send that 1-d array to conv1d2d_real.

          call get_netcdf_real_type (ncfile_id,chparm_gen(ip)
     &                              ,xtype,ignrret)

          if (xtype == 5) then
            call get_var3_tlev_real4 (ncfile_id,chparm_gen(ip),imax
     &                  ,jmax,ncix,f4,igvret)
            f = f4
          else
            call get_var3_tlev_double (ncfile_id,chparm_gen(ip),imax
     &                  ,jmax,ncix,f8,igvret)
            f = f8
          endif

          if (verb .ge. 3) then
            print *,' '
            print *,'After genesis read, parm= ',chparm_gen(ip),' ifh= '
     &             ,ifh,' lead time index= ',ltix(ifh),' parm# (ip) = '
     &             ,ip,' ncix= ',ncix,' igvret= ',igvret
          endif

          if (igvret == 0) then

c            call bitmapchk(kf,lb,f,dmin,dmax)

            readgenflag(ip) = .true.
            dmin = minval(f)
            dmax = maxval(f)

            nf_status = nf_inq_varid (ncfile_id,chparm_gen(ip),varid)

            call get_netcdf_real_type (ncfile_id,chparm_gen(ip)
     &                                ,xtype,ignrret)

            if (xtype == 5) then
              nf_status = nf_get_att_real (ncfile_id,varid
     &                    ,"missing_value",xmissing_val4)
              xmissing_value = xmissing_val4
            else
              nf_status = nf_get_att_double (ncfile_id,varid
     &                    ,"missing_value",xmissing_val8)
              xmissing_value = xmissing_val8
            endif

            if (verb .ge. 3) then
              write (6,331)
 331          format ('Genesis parmread lead time     parm#'
     &               ,'        parm_id   '
     &               ,23x,'minval       maxval')

              write (6,333) ifhours(ifh),ifclockmins(ifh),ip
     &                     ,chparm_gen(ip),dmin,dmax
 333          format ('   ',i3,':',i2.2,22x,i3,10x,a30,1x,2g12.4)
              write (6,335) chparm_gen(ip),xmissing_value
 335          format ('   --- ',a38,' missing value = ',g12.4)
            endif

            if (ip == 1) then   ! 850 mb specific humidity 
              call conv1d2d_real_netcdf (imax,jmax,f,q850(1,1)
     &                                   ,need_to_flip_lats)
            else if (ip == 2) then   ! 850 mb temperature
              call conv1d2d_real_netcdf (imax,jmax,f,t850(1,1)
     &                                   ,need_to_flip_lats)
            else if (ip == 3) then   ! 850 mb relative humidity
              call conv1d2d_real_netcdf (imax,jmax,f,rh850(1,1)
     &                                   ,need_to_flip_lats)
            else if (ip == 4) then   ! 1000 mb relative humidity 
              call conv1d2d_real_netcdf (imax,jmax,f,rh(1,1,1)
     &                                   ,need_to_flip_lats)
            else if (ip == 5) then   ! 925 mb relative humidity 
              call conv1d2d_real_netcdf (imax,jmax,f,rh(1,1,2)
     &                                   ,need_to_flip_lats)
            else if (ip == 6) then   ! 800 mb relative humidity 
              call conv1d2d_real_netcdf (imax,jmax,f,rh(1,1,3)
     &                                   ,need_to_flip_lats)
            else if (ip == 7) then   ! 750 mb relative humidity 
              call conv1d2d_real_netcdf (imax,jmax,f,rh(1,1,4)
     &                                   ,need_to_flip_lats)
            else if (ip == 8) then   ! 700 mb relative humidity 
              call conv1d2d_real_netcdf (imax,jmax,f,rh(1,1,5)
     &                                   ,need_to_flip_lats)
            else if (ip == 9) then   ! 650 mb relative humidity 
              call conv1d2d_real_netcdf (imax,jmax,f,rh(1,1,6)
     &                                   ,need_to_flip_lats)
            else if (ip == 10) then   ! 600 mb relative humidity 
              call conv1d2d_real_netcdf (imax,jmax,f,rh(1,1,7)
     &                                   ,need_to_flip_lats)
            else if (ip == 11) then   ! 1000 mb specific humidity
              call conv1d2d_real_netcdf (imax,jmax,f,spfh(1,1,1)
     &                                   ,need_to_flip_lats)
            else if (ip == 12) then   ! 925 mb specific humidity
              call conv1d2d_real_netcdf (imax,jmax,f,spfh(1,1,2)
     &                                   ,need_to_flip_lats)
            else if (ip == 13) then   ! 800 mb specific humidity
              call conv1d2d_real_netcdf (imax,jmax,f,spfh(1,1,3)
     &                                   ,need_to_flip_lats)
            else if (ip == 14) then   ! 750 mb specific humidity
              call conv1d2d_real_netcdf (imax,jmax,f,spfh(1,1,4)
     &                                   ,need_to_flip_lats)
            else if (ip == 15) then   ! 700 mb specific humidity
              call conv1d2d_real_netcdf (imax,jmax,f,spfh(1,1,5)
     &                                   ,need_to_flip_lats)
            else if (ip == 16) then   ! 650 mb specific humidity
              call conv1d2d_real_netcdf (imax,jmax,f,spfh(1,1,6)
     &                                   ,need_to_flip_lats)
            else if (ip == 17) then   ! 600 mb specific humidity
              call conv1d2d_real_netcdf (imax,jmax,f,spfh(1,1,7)
     &                                   ,need_to_flip_lats)
            else if (ip == 18) then   ! 1000 mb temperature
              call conv1d2d_real_netcdf (imax,jmax,f,temperature(1,1,1)
     &                                   ,need_to_flip_lats)
            else if (ip == 19) then   !  925 mb temperature
              call conv1d2d_real_netcdf (imax,jmax,f,temperature(1,1,2)
     &                                   ,need_to_flip_lats)
            else if (ip == 20) then   !  800 mb temperature
              call conv1d2d_real_netcdf (imax,jmax,f,temperature(1,1,3)
     &                                   ,need_to_flip_lats)
            else if (ip == 21) then   !  750 mb temperature
              call conv1d2d_real_netcdf (imax,jmax,f,temperature(1,1,4)
     &                                   ,need_to_flip_lats)
            else if (ip == 22) then   !  700 mb temperature
              call conv1d2d_real_netcdf (imax,jmax,f,temperature(1,1,5)
     &                                   ,need_to_flip_lats)
            else if (ip == 23) then   !  650 mb temperature
              call conv1d2d_real_netcdf (imax,jmax,f,temperature(1,1,6)
     &                                   ,need_to_flip_lats)
            else if (ip == 24) then   !  600 mb temperature
              call conv1d2d_real_netcdf (imax,jmax,f,temperature(1,1,7)
     &                                   ,need_to_flip_lats)
            else if (ip == 25) then   !  500 mb omega
              call conv1d2d_real_netcdf (imax,jmax,f,omega500(1,1)
     &                                   ,need_to_flip_lats)
            else
              if (verb >= 3) then
                print *,' '
                print *,'!!! NOTE: Genesis NetCDF Parm not recognized.'
                print *,'!!!       ip is > 25.... ip= ',ip
                print *,'!!!       Forecast time level = ',ifh
              endif
            endif

          else

            if (verb >= 3) then
              print *,' '
              print *,'ERROR: in getdata_netcdf, from call to either'
              print *,'get_var3_tlev_real4 or get_var3_tlev_double,'
              print *,'igvret= ',igvret
            endif

          endif

        enddo netcdf_gen_parm_loop

      endif

c     *------------------------------------------------------------*
c      NetCDF Read for vortex tilt diagnostics
c
c      If we are attempting to perform vortex tilt diagnostics, then 
c      read in data now that will allow us to do that.
c
c     *------------------------------------------------------------*

      if (vortex_tilt_flag == 'y') then

        if (allocated(cnc_tilt_var))     deallocate (cnc_tilt_var)
        if (allocated(cnc_tilt_var_prs)) deallocate (cnc_tilt_var_prs)
        allocate (cnc_tilt_var(num_vortex_tilt_levs,2),stat=ictvret)
        allocate (cnc_tilt_var_prs(num_vortex_tilt_levs),stat=ictvpret)

        if (ictvret /= 0 .or. ictvpret /= 0) then
          print *,' '
          print *,'!!! ERROR in getdata_netcdf allocating cnc_tilt_var'
     *           ,' arrays.'
          print *,'!!! ictvret= ',ictvret,' ictvpret= ',ictvpret
          print *,'!!! STOPPING....'
          stop 94
        endif

        if (verb >= 3) then
          print *,' '
          print *,'Vortex tilt: Reading in NetCDF variable names that'
          print *,'             will be needed to read in the actual'
          print *,'             data.  NetCDF variable names follow:'
          print *,' '
        endif

        ict = 1
        do while (.true.)

          read (iunit_ncvt_vars,405,end=130) iprslev,cvar

          ilevct = int((ict-1) / 2 + 1)

          ilevmod = mod(ict,2)
          if (ilevmod > 0) then
            ilevix = 1
          elseif (ilevmod == 0) then
            ilevix = 2
          endif

          cnc_tilt_var(ilevct,ilevix) = cvar
          cnc_tilt_var_prs(ilevct)    = iprslev

          if (verb >= 3) then
            write (6,417) ilevct,ilevix,iprslev
     &                   ,cnc_tilt_var(ilevct,ilevix)
          endif 

          ict = ict + 1

        enddo

  130   continue

  405   format (1x,i4,1x,a40)
  417   format (1x,'NetCDF tilt vars: ilevct= ',i3,'  ilevix= ',i4
     &            ,' prs_level= ',i4,'  cvar: ---> ',a40,'<----')

        rewind (iunit_ncvt_vars)

        nc_vortex_tilt_read_loop: do ip = 1,num_vortex_tilt_levs

          ! For each vertical level, two passes are made through the
          ! next loop.  If our vortex_tilt_parm is 'zeta' or 'wcirc',
          ! then we need to read both the u and v data, i.e., we need to
          ! go through two read iterations of this loop.  Otherwise, if
          ! our vortex_tilt_parm is 'hgt' or 'temp', then we only need
          ! to read one record per height level, so for the second
          ! iteration through this next nploop, just cycle.

          nploop: do np = 1,2

            if (np == 2 .and. (vortex_tilt_parm == 'hgt' .or.
     &                         vortex_tilt_parm == 'temp')) then
              cycle nploop
            endif

            call get_netcdf_real_type (ncfile_id
     &            ,trim(cnc_tilt_var(ip,np)),xtype,ignrret)

            if (xtype == 5) then
              call get_var3_tlev_real4 (ncfile_id
     &            ,trim(cnc_tilt_var(ip,np)),imax,jmax,ncix,f4,igvret)
              f = f4
            else
              call get_var3_tlev_double (ncfile_id
     &            ,trim(cnc_tilt_var(ip,np)),imax,jmax,ncix,f8,igvret)
              f = f8
            endif

            if (verb >= 3) then
              print *,' '
              print *,'After tilt read, parm= ',cnc_tilt_var(ip,np)
     &           ,' ifh= ',ifh,' lead time index= ',ltix(ifh)
     &           ,' np #  = ',np,' ncix= ',ncix,' igvret= ',igvret
            endif

            if (igvret == 0) then

              dmin = minval(f)
              dmax = maxval(f)

              nf_status = nf_inq_varid (ncfile_id
     &            ,trim(cnc_tilt_var(ip,np)),varid)

              call get_netcdf_real_type (ncfile_id
     &            ,trim(cnc_tilt_var(ip,np)),xtype,ignrret)

              if (xtype == 5) then
                nf_status = nf_get_att_real (ncfile_id,varid
     &                      ,"missing_value",xmissing_val4)
                xmissing_value = xmissing_val4
              else
                nf_status = nf_get_att_double (ncfile_id,varid
     &                      ,"missing_value",xmissing_val8)
                xmissing_value = xmissing_val8
              endif

              if (verb >= 3) then
                write (6,431)
 431            format (' V-tilt parmread lead time    lev#'
     &                 ,' pass#    parm_id   '
     &                 ,23x,' minval      maxval')

                write (6,433) ifhours(ifh),ifclockmins(ifh),ip,np
     &                       ,cnc_tilt_var(ip,np),dmin,dmax
 433            format ('   ',i3,':',i2.2,20x,i3,2x,i3,8x,a30,1x,2g12.4)
                write (6,435) cnc_tilt_var(ip,np),xmissing_value
 435            format ('   --- ',a38,' missing value = ',g12.4)
              endif

c             Convert data to 2-d array.  If the parameter we are
c             tracking for vortex tilt is zeta or wcirc, then we need
c             to first place the data into separate u and v arrays, then
c             we will compute zeta or wind circulation in a different
c             routine, and we will then store that zeta or wind
c             circulation data.

              if (vortex_tilt_parm == 'zeta' .or.
     &            vortex_tilt_parm == 'wcirc') then
                if (np == 1) then
                  call conv1d2d_real_netcdf (imax,jmax,f,utilt(1,1,ip)
     &                                      ,need_to_flip_lats)
                  utilt_readflag(ip) = .true.
                else
                  call conv1d2d_real_netcdf (imax,jmax,f,vtilt(1,1,ip)
     &                                      ,need_to_flip_lats)
                  vtilt_readflag(ip) = .true.
                endif
              else
                call conv1d2d_real_netcdf (imax,jmax,f,xtilt(1,1,ip)
     &                                    ,need_to_flip_lats)
                xtilt_readflag(ip) = .true.
              endif

            else

              if (verb >= 3) then
                print *,' '
                print *,'ERROR: in getdata_netcdf, from call to either'
                print *,'get_var3_tlev_real4 or get_var3_tlev_double'
                print *,'for vortex_tilt variables.'
                print *,'igvret= ',igvret,' ip= ',ip,' np= ',np
              endif

            endif

          enddo nploop

        enddo nc_vortex_tilt_read_loop

      endif

      if (allocated(f)) deallocate(f)
c
      return
      end
c
c------------------------------------------------------------------
c
c------------------------------------------------------------------
      subroutine create_bitmap_netcdf (imax,jmax,dat1d,lb1d
     &                                ,xmissing_val)
c
c     ABSTRACT: The purpose of this routine is to create a logical
c     bitmap to be used for masking out regions with missing data, 
c     such as for a regional grid with irregular boundaries (such as 
c     we've seen for the regional / nested FV3).  This bitmap will 
c     have the same functionality as a GRIB1/GRIB2 bitmap.  The trick
c     is that NetCDF does not have a logical bitmap within its 
c     definition, so we need to make one.  We do this by reading in 
c     the "missing_value" attribute for any variable, then here we 
c     scan through all the data values retrieved from the NetCDF read,
c     and then for all grid points with missing values we set the 
c     valid_pt flag to .false.
c
c     Note the use of the need_to_flip_lats flag.  This is in order to 
c     handle grids that are flipped.  Most grids -- NCEP, UKMET, ECMWF
c     -- have point (1,1) as the uppermost left point on the grid, and
c     the data goes from north to south.  Some grids -- GFDL and the 
c     new NAVGEM grid -- are flipped; their point (1,1) is the lowermost
c     left point, and their data goes from south to north.  So if
c     the need_to_flip_lats flag was set to TRUE in getgridinfo, meaning
c     that we have northward scanning data, we catch it in this
c     subroutine and flip the data ourselves for our own arrays,
c     since this whole program is structured around the data going
c     from north to south.
c
c     PARAMETERS:
c
c     INPUT:
c     imax     Number of gridpoints in i direction in input box
c     jmax     Number of gridpoints in j direction in input box
c     dat1d    1-d array containing floating point data values
c     xmissing_val real value of missing value for the given variable
c                  that was read in for the calling routine
c     need_to_flip_lats  logical flag, set in getgridinfo, that
c              indicates if data is correctly N-to-S, or if it is
c              S-to-N and needs to be flipped.
c
c     OUTPUT:
c     lb1d     1-d array containing logical bitmap values
c
      USE verbose_output

      implicit none

      logical(1) lb1d(imax*jmax)
      logical(1) need_to_flip_lats
      integer    ilat,ilatix,ilon,imax,jmax,tct,fct,mct,kf
      real ::  dat1d(imax*jmax)
      real ::  xmissing_val
c
      tct = 0
      fct = 0
      mct = 0

      kf = imax * jmax

      if (verb >= 3) then
        print *,' '
        print *,'TOP of conv1d2d_logic_netcdf, xmissing_val= '
     &         ,xmissing_val,' kf= ',kf
        print *,' '
      endif
c
      do k = 1,kf
        if (dat1d(k) == xmissing_val) then
          lb1d(k) = .false.
          fct = fct + 1
        else
          lb1d(k) = .true.
          fct = fct + 1
        endif
      enddo
c
      print *,' '
      print *,' LB STATS: tct= ',tct,' fct= ',fct,' mct= ',mct
c
      return
      end
c
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine read_netcdf_hours (ncfile,ncfile_id,ncfile_tmax,ifhmax
     &                            ,ncfile_has_hour0,netcdfinfo,irnhret)
c
c     ABSTRACT: The purpose of this subroutine is to read the "time"
c     dimension and "time data" from the NetCDF file so that we know
c     how many time levels there are and what those time levels are.
c     One reason for doing this is that some models, like the old GFDL
c     FV3, do not output hour 0 data, so we need to check this first
c     before running through the tracking processing for the various 
c     hours.  We will take the list of hours read in here directly from
c     the NetCDF file and compare that against the *requested* list of 
c     forecast hours that the user has entered.  The user might not be 
c     aware that there is no hour 0 data for a given model.  We compare
c     these two lists of forecast hours and then write a message if 
c     there is a lead time that is not in the NetCDF file.
c   
c     INPUT:
c     ncfile     character name of NetCDF file
c     ncfile_id  integer id associated with NetCDF file after open
c     ifhmax     integer max number of lead times that the user has 
c                requested on the input lead times data file.  This
c                value was set in subroutine read_fhours.
c     netcdfinfo variable of user-defined type netcdfstuff (from 
c                module netcdf_parms).
c
c     OUTPUT:
c     ncfile_tmax integer max number of lead times that are in the 
c                 NetCDF file, as read in from this subroutine
c     ncfile_has_hour0 character flag (y|n) that tells whether or not 
c                 the input NetCDF data file actually has an hour0 
c                 record in it or not.
c
      USE netcdf_parms

      implicit none
c
      type (netcdfstuff) netcdfinfo

      character :: ncfile*180,ncfile_has_hour0*1,match_check*1
      real(kind=4), allocatable :: temp_nc_time_vals_r4(:)
      real(kind=8), allocatable :: temp_nc_time_vals_r8(:)
      integer, intent(in)  :: ncfile_id
      integer, intent(out) :: ncfile_tmax
      integer :: infta,k,m,n,ifhmax,irnhret,usertime,xtype,ignrret
c

      irnhret = 0
      ncfile_has_hour0 = 'n'

      !-----------------------------------------------------------
      ! First read the NetCDF file to get the number of time levels,
      ! which will be returned in "ncfile_tmax"....
      !-----------------------------------------------------------

      print *,' '
      print *,'in read_netcdf_hours...'
      print *,'ncfile_id= ',ncfile_id
      print *,'netcdfinfo%time_name= ',netcdfinfo%time_name
      print *,'ncfile_tmax= ',ncfile_tmax

      call get_ncdim1(ncfile_id,netcdfinfo%time_name,ncfile_tmax)

      if (verb .ge. 1) then
        print *,'in getgridinfo_netcdf, ncfile_id= ',ncfile_id
        print *,'Num netcdf time levs=  ncfile_tmax= ',ncfile_tmax
      endif

      if (allocated(netcdf_file_time_values)) then
        deallocate (netcdf_file_time_values)
      endif

      if (allocated(temp_nc_time_vals_r4)) then
        deallocate (temp_nc_time_vals_r4)
      endif

      if (allocated(temp_nc_time_vals_r8)) then
        deallocate (temp_nc_time_vals_r8)
      endif

      allocate (netcdf_file_time_values(ncfile_tmax),stat=infta)
      if (infta /= 0) then
        print *,' '
        print *,'!!! ERROR in sub read_netcdf_hours allocating'
        print *,'!!! netcdf_file_time_values array.  infta = ',infta
        irnhret = 94
        return
      endif

      allocate (temp_nc_time_vals_r4(ncfile_tmax),stat=infta)
      if (infta /= 0) then
        print *,' '
        print *,'!!! ERROR in sub read_netcdf_hours allocating'
        print *,'!!! temp_nc_time_vals_r4 array.  infta = ',infta
        irnhret = 94
        return
      endif

      allocate (temp_nc_time_vals_r8(ncfile_tmax),stat=infta)
      if (infta /= 0) then
        print *,' '
        print *,'!!! ERROR in sub read_netcdf_hours allocating'
        print *,'!!! temp_nc_time_vals_r8 array.  infta = ',infta
        irnhret = 94
        return
      endif


      !-----------------------------------------------------------
      ! Now read in the actual time values that are stored in the 
      ! NetCDF file....
      !-----------------------------------------------------------

      call get_netcdf_real_type (ncfile_id,netcdfinfo%time_name
     &                                  ,xtype,ignrret)

      if (xtype == 5) then
        call get_var1_one_dim4 (ncfile_id,netcdfinfo%time_name
     &                      ,ncfile_tmax,temp_nc_time_vals_r4)
        netcdf_file_time_values = temp_nc_time_vals_r4
      else
        call get_var1_one_dim8 (ncfile_id,netcdfinfo%time_name
     &                      ,ncfile_tmax,temp_nc_time_vals_r8)
        netcdf_file_time_values = temp_nc_time_vals_r8
      endif

      if (verb .ge. 1) then
        do k = 1,ncfile_tmax
          print *,'k= ',k,' netcdf_file_time_values(k)= '
     &                     ,netcdf_file_time_values(k)
        enddo
      endif

      if (allocated(temp_nc_time_vals_r4)) then
        deallocate (temp_nc_time_vals_r4)
      endif

      if (allocated(temp_nc_time_vals_r8)) then
        deallocate (temp_nc_time_vals_r8)
      endif

      !------------------------------------------------------------
      ! Now convert the NetCDF time values into hours in order to 
      ! be able to compare with the user-requested list of lead 
      ! times.  Remember that the NetCDF lead times will be listed
      ! either as hours or as fractions of days.
      !------------------------------------------------------------

      if (allocated(nctotalmins)) then
        deallocate (nctotalmins)
      endif 

      allocate (nctotalmins(ncfile_tmax),stat=infta)
      if (infta /= 0) then
        print *,' ' 
        print *,'!!! ERROR in sub read_netcdf_hours allocating '
        print *,'!!! nctotalmins array.  infta = ',infta
        irnhret = 94
        return
      endif

      if (allocated(nctotalhours)) then
        deallocate (nctotalhours)
      endif
              
      allocate (nctotalhours(ncfile_tmax),stat=infta)
      if (infta /= 0) then
        print *,' ' 
        print *,'!!! ERROR in sub read_netcdf_hours allocating '
        print *,'!!! nctotalhours array.  infta = ',infta
        irnhret = 94
        return
      endif

      do k = 1,ncfile_tmax

        if (netcdfinfo%time_units == 'hours') then
          nctotalmins(k)  = int(netcdf_file_time_values(k)) * 60
          nctotalhours(k) = int(netcdf_file_time_values(k))
        elseif (netcdfinfo%time_units == 'days') then
          nctotalmins(k)  = int(netcdf_file_time_values(k) * 60. * 24.)
          nctotalhours(k) = int(netcdf_file_time_values(k) * 24.)
        else
          print *,' '
          print *,'!!! ERROR: In read_netcdf_hours, the value of'
          print *,'    netcdfinfo%time_units is neither hours nor days.'
          print *,'    netcdfinfo%time_units= ',netcdfinfo%time_units
          print *,'    STOPPING....'
          print *,' '
          stop 99
        endif
 
        if (verb .ge. 1) then
          write (6,71) k,netcdf_file_time_values(k),nctotalmins(k)
     &                ,nctotalhours(k)
        endif

      enddo

   71 format (1x,i5,'  netcdf_file_time_values(k)= ',f8.4
     &             ,'  nctotalmins(k)= ',i10
     &             ,'     nctotalhours(k)= ',i10)

      !------------------------------------------------------------
      ! Now go through the list of user-requested lead times that 
      ! were read in from subroutine read_fhours and try to match
      ! the two lists up.  The big one to watch out for is whether
      ! or not the NetCDF file actually has an hour 0 lead time.
      !------------------------------------------------------------

      usertime = ifcsthour

      match_check = 'n'

      netcdfloop: do m = 1,ncfile_tmax

        if (usertime == nctotalhours(m)) then
          if (verb .ge. 1) then
            print *,'+++ Time match for usertime= ',usertime
          endif
          match_check = 'y'
        endif
        
      enddo netcdfloop 

      if (match_check == 'n') then

        if (usertime == 0) then
          print *,' '
          print *,'Warning: For a NetCDF file, the user has requested'
          print *,'to read in an hour 0 file, however a scan of the'
          print *,'time data values in the NetCDF file indicates'
          print *,'that there is no hour 0 data in this file. '
          print *,'We will substitute either missing values or '
          print *,'the values from the TC Vitals data in the '
          print *,'hour 0 record and then start searching at the '
          print *,'next lead time.'
          ncfile_has_hour0 = 'n'
        else
          print *,' '
          print *,'!!! ERROR: For a NetCDF file, the user has'
          print *,'  requested to process a particular lead time that'
          print *,'  does not exist in the NetCDF list of time '
          print *,'  values.'
          print *,'  n= ',n
          print *,'  usertime= ifcsthour= ',ifcsthour
          print *,'  STOPPING....'
          stop 99
        endif

      elseif (match_check == 'y') then

        if (usertime == 0) then
          if (verb .ge. 1) then
            print *,' ' 
            print *,'+++ For the input NetCDF file, an hour0 data '
            print *,'    record exists in the data file.'
          endif
          ncfile_has_hour0 = 'y'
        endif

      endif
c
      return
      end
c
c------------------------------------------------------------------
c
c------------------------------------------------------------------
      subroutine get_ncdim1 (ncid,var1_name,nmax)
c    
c     ABSTRACT: This routine queries a netcdf file to get the
c     value of a requested file dimension (e.g., imax, jmax)
c         
      implicit none    
     
      include "netcdf.inc"
                                
      integer,       intent(in)  :: ncid
      character*(*), intent(in)  :: var1_name
      integer,       intent(out) :: nmax
      integer                    :: status, var1id
          
      status = nf_inq_dimid (ncid,var1_name,var1id)
      if (status .ne. NF_NOERR) call handle_netcdf_err(status)
          
      status = nf_inq_dimlen (ncid,var1id,nmax)
      if (status .ne. NF_NOERR) call handle_netcdf_err(status)
          
      end subroutine get_ncdim1
c
c---------------------------------------------------------
c     
c---------------------------------------------------------
      subroutine get_netcdf_real_type (ncid,var3_name,xtype,ignrret)
c         
c     ABSTRACT: This routine inquires into a NetCDF file using calls 
c     to the NetCDF library to determine the real type (32-bit vs.
c     64-bit real) for a given variable.
c         
c     PARAMETERS
c       
c     INPUT:
c     ncid   integer that contains the NetCDF file ID
c     var3_name  character name of NetCDF input variable
c     
c     OUTPUT:
c     xtype  integer value that indicates 4-byte or 8-byte real.
c            A value of 5 = 4-byte real;  6 = 8-byte real.
c     ignrret integer return code from this routine
      
      USE tracked_parms; USE verbose_output; USE netcdf_parms

      implicit         none

      include "netcdf.inc"
      integer, intent(in)       :: ncid
      character*(*), intent(in) :: var3_name
      integer                   :: xtype
      integer :: status,var3id,ignrret

      if (verb .ge. 3) then
        print *,' '
        print *,'In get_netcdf_real_type, ncid=  ',ncid
      endif  

      status = nf_inq_varid (ncid,var3_name,var3id)

      if (status /= NF_NOERR) then
        print *,' ' 
        print *,'NOTE: Could not find variable ',var3_name 
     &         ,' in NetCDF file ID= ncid= ',ncid
        ignrret = 92
        return
      endif

      status = nf_inq_vartype (ncid, var3id, xtype)
      if (status .ne. NF_NOERR) call handle_netcdf_err(status)

      if (xtype == 5 .or. xtype == 6) then 
        continue
      else
        if (verb >= 1) then
          print *,' '
          print *,'!!! ERROR: xtype returned in get_netcdf_real_type is'
          print *,'           not equal to 5 or 6.  xtype= ',xtype
          print *,'    EXITING....'
          print *,' '
        endif
        STOP 91
      endif
c
      return
      end subroutine get_netcdf_real_type
c
c------------------------------------------------------------------
c
c------------------------------------------------------------------
      subroutine get_var1_one_dim4 (ncid,var1_name,nmax,readvar4)
c    
c     ABSTRACT: This routine reads a netcdf file in order to return
c     a 1-dimensional array of data.  This one is intended for an
c     array of real, 4-byte data.
     
      USE verbose_output
      
      implicit         none
      
      include "netcdf.inc"
          
      integer, intent(in):: ncid
      character*(*), intent(in)::  var1_name
      integer, intent(in):: nmax
      integer    :: ira
      real(kind=4), intent(out) :: readvar4(nmax)
        
      integer ::  status, var1id
        
      status = nf_inq_varid (ncid,var1_name,var1id)
      if (status .ne. NF_NOERR) call handle_netcdf_err(status)
!     write(*,*) 'Got var1id', var1id
        
      ! Read data into a 4-byte real array
      status = nf_get_var_real (ncid,var1id,readvar4)
      if (status .ne. NF_NOERR) call handle_netcdf_err(status)
c
      end subroutine get_var1_one_dim4
c
c------------------------------------------------------------------
c
c------------------------------------------------------------------
      subroutine get_var1_one_dim8 (ncid,var1_name,nmax,readvar8)
c
c     ABSTRACT: This routine reads a netcdf file in order to return
c     a 1-dimensional array of data.  This one is intended for an
c     array of real, 8-byte data.

      USE verbose_output

      implicit         none

      include "netcdf.inc"

      integer, intent(in):: ncid
      character*(*), intent(in)::  var1_name
      integer, intent(in):: nmax
      integer    :: ira
      real(kind=8), intent(out) :: readvar8(nmax)

      integer ::  status, var1id

      status = nf_inq_varid (ncid,var1_name,var1id)
      if (status .ne. NF_NOERR) call handle_netcdf_err(status)
!     write(*,*) 'Got var1id', var1id

      ! Read data into an 8-byte real array
      status = nf_get_var_double (ncid,var1id,readvar8)
      if (status .ne. NF_NOERR) call handle_netcdf_err(status)
c
      end subroutine get_var1_one_dim8
c
c---------------------------------------------------------
c         
c---------------------------------------------------------
      subroutine get_var3_tlev_real4 (ncid,var3_name,imax,jmax,ncix
     &                         ,var3,igvret)
c
c     ABSTRACT: This routine reads a netcdf file and returns a 
c     2-dimensional synoptic variable at a particular lead time.
c     The lead time is specified by the ltix array, which is 
c     included in module tracked_parms and defined in subroutine 
c     read_fhours.
c
c     PARAMETERS
c
c     INPUT:
c     ncid   integer that contains the NetCDF file ID
c     var3_name  character name of NetCDF input file
c     imax   integer x-dimension of input data
c     jmax   integer y-dimension of input data
c     ncix   integer index of time level for where this time level 
c            actually is inside the NetCDF data.  Do NOT confuse this 
c            with the index of where this forecast hour is in the 
c            user's list of input forecast hours, as they may be
c            different.  For example, the user may request times that
c            are every 6 hours, but the NetCDF file might have times
c            that are every hour, so the indices for those two arrays
c            will be different.  Be sure to use the one (ncix) that 
c            indicates where the data actually starts in the 
c            NetCDF file.
c
c     OUTPUT:
c     var3   real array with real values returned from NetCDF read
c     igvret integer return code from this routine

      USE tracked_parms; USE verbose_output; USE netcdf_parms

      implicit         none

      include "netcdf.inc"
c
      integer, intent(in)       :: ncid,ncix
      character*(*), intent(in) :: var3_name
      integer, intent(in)       :: imax,jmax
      integer                   :: xtype
      real(kind=4), intent(out)   :: var3(kf)
      integer :: istart(3),ilength(3)
      integer :: status,var3id,igvret

      if (verb .ge. 3) then
        print *,' '
        print *,'In get_var3_tlev_double, ncix=  ',ncix
        print *,' nctotalmins(ncix)= ',nctotalmins(ncix)
      endif

      istart(1) = 1
      istart(2) = 1
      istart(3) = ncix

      ilength(1) = imax
      ilength(2) = jmax
      ilength(3) = 1

      igvret = 0

      status = nf_inq_varid (ncid,var3_name,var3id)

      if (status /= NF_NOERR) then
        print *,' '
        print *,'NOTE: Could not find variable ',var3_name,' at time'
     &         ,' index ncix= ',ncix
     &         ,' nctotalmins(ncix)= ',nctotalmins(ncix)

        igvret = 92
        return
      endif

      status = nf_get_vara_real (ncid,var3id,istart,ilength,var3)
      if (status .ne. NF_NOERR) call handle_netcdf_err(status)
c
      end subroutine get_var3_tlev_real4
c
c---------------------------------------------------------
c         
c---------------------------------------------------------
      subroutine get_var3_tlev_double (ncid,var3_name,imax,jmax,ncix
     &                         ,var3,igvret)
c
c     ABSTRACT: This routine reads a netcdf file and returns a 
c     2-dimensional synoptic variable at a particular lead time.
c     The lead time is specified by the ltix array, which is 
c     included in module tracked_parms and defined in subroutine 
c     read_fhours.  This routine is designed for returning an 
c     array of double-precision reals.
c
c     PARAMETERS
c
c     INPUT:
c     ncid   integer that contains the NetCDF file ID
c     var3_name  character name of NetCDF input file
c     imax   integer x-dimension of input data
c     jmax   integer y-dimension of input data
c     ncix   integer index of time level for where this time level 
c            actually is inside the NetCDF data.  Do NOT confuse this 
c            with the index of where this forecast hour is in the 
c            user's list of input forecast hours, as they may be
c            different.  For example, the user may request times that
c            are every 6 hours, but the NetCDF file might have times
c            that are every hour, so the indices for those two arrays
c            will be different.  Be sure to use the one (ncix) that 
c            indicates where the data actually starts in the 
c            NetCDF file.
c
c     OUTPUT:
c     var3   real array with real values returned from NetCDF read
c     igvret integer return code from this routine

      USE tracked_parms; USE verbose_output; USE netcdf_parms

      implicit         none

      include "netcdf.inc"
c
      integer, intent(in)       :: ncid,ncix
      character*(*), intent(in) :: var3_name
      integer, intent(in)       :: imax,jmax
      integer                   :: xtype
      real(kind=8), intent(out)   :: var3(imax,jmax)
      integer :: istart(3),ilength(3)
      integer :: status,var3id,igvret

      if (verb .ge. 3) then
        print *,' '
        print *,'In get_var3_tlev_double, ncix=  ',ncix
        print *,' nctotalmins(ncix)= ',nctotalmins(ncix)
      endif

      istart(1) = 1
      istart(2) = 1
      istart(3) = ncix

      ilength(1) = imax
      ilength(2) = jmax
      ilength(3) = 1

      igvret = 0

      status = nf_inq_varid (ncid,var3_name,var3id)

      if (status /= NF_NOERR) then
        print *,' '
        print *,'NOTE: Could not find variable ',var3_name,' at time'
     &         ,' index ncix= ',ncix
     &         ,' nctotalmins(ncix)= ',nctotalmins(ncix)

        igvret = 92
        return
      endif

      status = nf_get_vara_double (ncid,var3id,istart,ilength,var3)
      if (status .ne. NF_NOERR) call handle_netcdf_err(status)

      end subroutine get_var3_tlev_double
c
c----------------------------------------------------------------------
c
c----------------------------------------------------------------------
      subroutine handle_netcdf_err (status)
c
c     ABSTRACT: This subroutine is an error handling routine for NetCDF-
c     related functions.

      implicit         none

      include          "netcdf.inc"

      integer          status
c
      if (status /= nf_noerr) then
        print *,' '
        print *,'Tracker NetCDF error: '
        print *, nf_strerror(status)
        stop 'Stopped'
      endif

      end subroutine handle_netcdf_err
c
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine average_data (kf,valid_pt,nlevsin,ilevs,readflag
     &                 ,xinptmp,xouttmp,iidret)
c
c     ABSTRACT: This routine averages data between 300 and 500 mb to get
c     a mean temperature at 400 mb.  The input data should be at 50 mb 
c     resolution, giving 5 input levels in total.

      implicit none

      logical(1)   valid_pt(kf),readflag(nlevsin)
      integer      ilevs(nlevsin)
      integer      nlevsin,kf,k,n,iidret
      real         xinptmp(kf,nlevsin),xouttmp(kf)
      real         xinlevs_p(nlevsin),xinlevs_lnp(nlevsin)
      real         xsum
c
      iidret=0
      print *,'*----------------------------------------------*'
      print *,'         Top of average data routine'
      print *,'*----------------------------------------------*'
      print *,' '

      do n = 1,kf
        xsum = 0.0
c        print *,' '
        do k = 1,nlevsin
          xsum = xsum + xinptmp(n,k)
c          print *,'n= ',n,' k= ',k,' xsum= ',xsum
        enddo
        xouttmp(n) = xsum / float(nlevsin)
c        print *,'n= ',n,' mean= ',xouttmp(n)
      enddo
c
      return
      end
c
c----------------------------------------------------------------------
c
c----------------------------------------------------------------------
      subroutine output_data (lout,kf,kpds,kgds,holdgfld,xouttmp
     &             ,valid_pt,xoutlev,nlevsout,gribver,ifcsthour,iodret)
c
c     ABSTRACT: This routine writes out the  output data on the 
c     specified output pressure levels.

      USE params
      USE grib_mod

      implicit none

      CHARACTER(len=1),pointer,dimension(:) :: cgrib
c      CHARACTER(len=1),pointer,allocatable :: cgrib(:)
      type(gribfield) :: holdgfld
      logical(1) valid_pt(kf),bmap(kf)
      integer  lout,kf,lugb,lugi,iodret,nlevsout,igoret,ipret,lev
      integer  gribver,ierr,ipack,lengrib,npoints,newlen,idrsnum
      integer  numcoord,ica,n,j,ifcsthour
      integer :: idrstmpl(200)
      integer :: currlen=1000000
      integer :: listsec0(2)=(/0,2/)
      integer :: igds(5)=(/0,0,0,0,0/),previgds(5)
      integer  kpds(200),kgds(200)
      integer(4), parameter::idefnum=1
      integer(4) ideflist(idefnum),ibmap
      real     xouttmp(kf),xoutlev,coordlist
c
      iodret=0
      call baopenw (lout,"fort.51",igoret)
      print *,'baopenw: igoret= ',igoret

      if (igoret /= 0) then
        print *,' '
        print *,'!!! ERROR in sub output_data opening'
        print *,'!!! **OUTPUT** grib file.  baopenw return codes:'
        print *,'!!! grib file 1 return code = igoret = ',igoret
        STOP 95
        return
      endif

      if (gribver == 2) then

        ! Write data out as a GRIB2 message....

        allocate(cgrib(currlen),stat=ica)
        if (ica /= 0) then
          print *,' '
          print *,'ERROR in output_data allocating cgrib'
          print *,'ica=  ',ica
          iodret=95
          return
        endif
          

        !  Ensure that cgrib array is large enough
        
        if (holdgfld%ifldnum == 1 ) then     ! start new GRIB2 message
           npoints=holdgfld%ngrdpts
        else
           npoints=npoints+holdgfld%ngrdpts
        endif
        newlen=npoints*4
        if ( newlen.gt.currlen ) then
ccc          if (allocated(cgrib)) deallocate(cgrib)
          if (associated(cgrib)) deallocate(cgrib)
          allocate(cgrib(newlen),stat=ierr)
c          call realloc (cgrib,currlen,newlen,ierr)
          if (ierr == 0) then
            print *,' '
            print *,'re-allocate for large grib msg: '
            print *,'  currlen= ',currlen
            print *,'  newlen=  ',newlen
            currlen=newlen
          else
            print *,'ERROR returned from 2nd allocate cgrib = ',ierr
            stop 95
          endif
        endif

        !  Create new GRIB Message
        listsec0(1)=holdgfld%discipline
        listsec0(2)=holdgfld%version

        print *,'output, holdgfld%idsectlen= ',holdgfld%idsectlen
        do j = 1,holdgfld%idsectlen
          print *,'     sect1, j= ',j,' holdgfld%idsect(j)= '
     &           ,holdgfld%idsect(j)
        enddo

        call gribcreate(cgrib,currlen,listsec0,holdgfld%idsect,ierr)
        if (ierr.ne.0) then
           write(6,*) ' ERROR creating new GRIB2 field (gribcreate)= '
     &                ,ierr
           stop 95
        endif

        previgds=igds
        igds(1)=holdgfld%griddef
        igds(2)=holdgfld%ngrdpts
        igds(3)=holdgfld%numoct_opt
        igds(4)=holdgfld%interp_opt
        igds(5)=holdgfld%igdtnum

        if (igds(3) == 0) then
          ideflist = 0
        endif

        call addgrid (cgrib,currlen,igds,holdgfld%igdtmpl
     &               ,holdgfld%igdtlen,ideflist,idefnum,ierr)

        if (ierr.ne.0) then
          write(6,*) ' ERROR from addgrid adding GRIB2 grid = ',ierr
          stop 95
        endif


        holdgfld%ipdtmpl(12) = int(xoutlev) * 100 

        ipack      = 40
        idrsnum    = ipack
        idrstmpl   = 0

        idrstmpl(2)= holdgfld%idrtmpl(2)
        idrstmpl(3)= holdgfld%idrtmpl(3)
        idrstmpl(6)= 0
        idrstmpl(7)= 255

        numcoord=0
        coordlist=0.0  ! Only needed for hybrid vertical coordinate,
                       ! not here, so set it to 0.0

       ! 0   - A bit map applies to this product and is specified in
       ! this section
       ! 255 - A bit map does not apply to this product
       ibmap=255     ! Bitmap indicator (see Code Table 6.0)

       print *,' '
       print *,'output, holdgfld%ipdtlen= ',holdgfld%ipdtlen
       do n = 1,holdgfld%ipdtlen
         print *,'output, n= ',n,' holdgfld%ipdtmpl= '
     &          ,holdgfld%ipdtmpl(n)
       enddo

       print *,'output, kf= ',kf

c       if (ifcsthour < 6) then
c         do n = 1,kf
cc           print *,'output, n= ',n,' xouttmp(n)= ',xouttmp(n)
c           write (92,151) n,xouttmp(n)
c  151      format (1x,'n= ',i6,'  xouttmp(n)= ',f10.4)
c         enddo
c       endif

       call addfield (cgrib,currlen,holdgfld%ipdtnum,holdgfld%ipdtmpl
     &               ,holdgfld%ipdtlen,coordlist
     &               ,numcoord
     &               ,idrsnum,idrstmpl,200
     &               ,xouttmp,kf,ibmap,bmap,ierr)

        if (ierr /= 0) then
          write(6,*) ' ERROR from addfield adding GRIB2 data = ',ierr
          stop 95
        endif

!       Finalize  GRIB message after all grids
!       and fields have been added.  It adds the End Section ( "7777" )

        call gribend(cgrib,currlen,lengrib,ierr)
        call wryte(lout,lengrib,cgrib)

        if (ierr == 0) then
          print *,' '
          print *,'+++ GRIB2 write successful. '
          print *,'    Len of message = currlen= ',currlen
          print *,'    Len of entire GRIB2 message = lengrib= ',lengrib
        else
          print *,' ERROR from gribend writing GRIB2 msg = ',ierr
          stop 95
        endif

      else

        ! Write data out as a GRIB1 message....

        kpds(6) = 100

        do lev = 1,nlevsout 

          kpds(7) = int(xoutlev)

          print *,'tave:  just before call to putgb, kf= ',kf

          print *,'output, kf= ',kf
c          do n = 1,kf
c            print *,'output, n= ',n,' xouttmp(n)= ',xouttmp(n)
c          enddo

          if (ifcsthour < 6) then
ctpm            do n = 1,kf
ctpmc              print *,'output, n= ',n,' xouttmp(n)= ',xouttmp(n)
ctpm              write (91,161) n,xouttmp(n)
ctpm  161         format (1x,'n= ',i6,'  xouttmp(n)= ',f10.4)
ctpm            enddo
            print *,' '
          endif

          call putgb (lout,kf,kpds,kgds,valid_pt,xouttmp,ipret)
          print *,'tave:  just after call to putgb, kf= ',kf
          if (ipret == 0) then
            print *,' '
            print *,'+++ IPRET = 0 after call to putgb'
            print *,' '
          else
            print *,' '
            print *,'!!!!!! ERROR in tave'
            print *,'!!!!!! ERROR: IPRET NE 0 AFTER CALL TO PUTGB !!!'
            print *,'!!!!!!        Level index= ',lev
            print *,'!!!!!!           pressure= ',xoutlev
            print *,' '
          endif

          write(*,980) kpds(1),kpds(2)
          write(*,981) kpds(3),kpds(4)
          write(*,982) kpds(5),kpds(6)
          write(*,983) kpds(7),kpds(8)
          write(*,984) kpds(9),kpds(10)
          write(*,985) kpds(11),kpds(12)
          write(*,986) kpds(13),kpds(14)
          write(*,987) kpds(15),kpds(16)
          write(*,988) kpds(17),kpds(18)
          write(*,989) kpds(19),kpds(20)
          write(*,990) kpds(21),kpds(22)
          write(*,991) kpds(23),kpds(24)
          write(*,992) kpds(25)
          write(*,880) kgds(1),kgds(2)
          write(*,881) kgds(3),kgds(4)
          write(*,882) kgds(5),kgds(6)
          write(*,883) kgds(7),kgds(8)
          write(*,884) kgds(9),kgds(10)
          write(*,885) kgds(11),kgds(12)
          write(*,886) kgds(13),kgds(14)
          write(*,887) kgds(15),kgds(16)
          write(*,888) kgds(17),kgds(18)
          write(*,889) kgds(19),kgds(20)
          write(*,890) kgds(21),kgds(22)

        enddo

  980   format('    kpds(1)  = ',i7,'  kpds(2)  = ',i7)
  981   format('    kpds(3)  = ',i7,'  kpds(4)  = ',i7)
  982   format('    kpds(5)  = ',i7,'  kpds(6)  = ',i7)
  983   format('    kpds(7)  = ',i7,'  kpds(8)  = ',i7)
  984   format('    kpds(9)  = ',i7,'  kpds(10) = ',i7)
  985   format('    kpds(11) = ',i7,'  kpds(12) = ',i7)
  986   format('    kpds(13) = ',i7,'  kpds(14) = ',i7)
  987   format('    kpds(15) = ',i7,'  kpds(16) = ',i7)
  988   format('    kpds(17) = ',i7,'  kpds(18) = ',i7)
  989   format('    kpds(19) = ',i7,'  kpds(20) = ',i7)
  990   format('    kpds(21) = ',i7,'  kpds(22) = ',i7)
  991   format('    kpds(23) = ',i7,'  kpds(24) = ',i7)
  992   format('    kpds(25) = ',i7)
  880   format('    kgds(1)  = ',i7,'  kgds(2)  = ',i7)
  881   format('    kgds(3)  = ',i7,'  kgds(4)  = ',i7)
  882   format('    kgds(5)  = ',i7,'  kgds(6)  = ',i7)
  883   format('    kgds(7)  = ',i7,'  kgds(8)  = ',i7)
  884   format('    kgds(9)  = ',i7,'  kgds(10) = ',i7)
  885   format('    kgds(11) = ',i7,'  kgds(12) = ',i7)
  886   format('    kgds(13) = ',i7,'  kgds(14) = ',i7)
  887   format('    kgds(15) = ',i7,'  kgds(16) = ',i7)
  888   format('    kgds(17) = ',i7,'  kgds(18) = ',i7)
  889   format('    kgds(19) = ',i7,'  kgds(20) = ',i7)
  890   format('    kgds(20) = ',i7,'  kgds(22) = ',i7)

      endif
c
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine open_grib_files (lugb,lugi,lout,gribver,iret)

C     ABSTRACT: This subroutine must be called before any attempt is
C     made to read from the input GRIB files.  The GRIB and index files
C     are opened with a call to baopenr.  This call to baopenr was not
C     needed in the cray version of this program (the files could be
C     opened with a simple Cray assign statement), but the GRIB-reading
C     utilities on the SP do require calls to this subroutine (it has
C     something to do with the GRIB I/O being done in C on the SP, and
C     the C I/O package needs an explicit open statement).
C
C     INPUT:
C     lugb     The Fortran unit number for the GRIB data file
C     lugi     The Fortran unit number for the GRIB index file
C     lout     The Fortran unit number for the  output grib file
c     gribver  integer (1 or 2) to indicate if using GRIB1 / GRIB2
C
C     OUTPUT:
C     iret     The return code from this subroutine

      implicit none

      character fnameg*255,fnamei*255,fnameo*255
      character enameb*16,enamei*16,enameo*16
      character lugb_c*16,lugi_c*16,lout_c*16
      integer   iret,gribver,lugb,lugi,lout,igoret,iioret,iooret

      iret=0
      write(lugb_c,'(I2.2)')lugb
      write(lugi_c,'(I2.2)')lugi
      write(lout_c,'(I2.2)')lout
      enameb='FORT'//lugb_c
      enamei='FORT'//lugi_c
      enameo='FORT'//lout_c
      call get_environment_variable(trim(enameb), fnameg, status=igoret)
      call get_environment_variable(trim(enamei), fnamei, status=iioret)
      call get_environment_variable(trim(enameo), fnameo, status=iooret)
      if (igoret /= 0 .or. iioret /= 0 .or. iooret /= 0) then
        fnameg(1:5) = "fort."
        fnamei(1:5) = "fort."
        fnameo(1:5) = "fort."
        write(fnameg(6:7),'(I2)') lugb
        write(fnamei(6:7),'(I2)') lugi
        write(fnameo(6:7),'(I2)') lout
      endif
      call baopenr (lugb,fnameg,igoret)
      call baopenr (lugi,fnamei,iioret)
      call baopenw (lout,fnameo,iooret)

      print *,' '
      print *,'tave baopen: igoret= ',igoret,' iioret= ',iioret
     &       ,' iooret= ',iooret
      
      if (igoret /= 0 .or. iioret /= 0 .or. iooret /= 0) then
        print *,' '
        print *,'!!! ERROR in tave'
        print *,'!!! ERROR in sub open_grib_files opening grib file'
        print *,'!!! or grib index file.  baopen return codes:'
        print *,'!!! grib  file return code = igoret = ',igoret
        print *,'!!! index file return code = iioret = ',iioret
        print *,'!!! output file return code = iooret = ',iooret
        iret = 93
        return
      endif
      
      return
      end
c
c-------------------------------------------------------------------
c
c-------------------------------------------------------------------
      subroutine bitmapchk (n,ld,d,dmin,dmax)
c
c     This subroutine checks the bitmap for non-existent data values.
c     Since the data from the regional models have been interpolated
c     from either a polar stereographic or lambert conformal grid
c     onto a lat/lon grid, there will be some gridpoints around the
c     edges of this lat/lon grid that have no data; these grid
c     points have been bitmapped out by Mark Iredell's interpolater.
c     To provide another means of checking for invalid data points
c     later in the program, set these bitmapped data values to a
c     value of -999.0.  The min and max of this array are also
c     returned if a user wants to check for reasonable values.
c
      logical(1) ld
      dimension  ld(n),d(n)
c
      dmin=1.E15
      dmax=-1.E15
c
      do i=1,n
        if (ld(i)) then
          dmin=min(dmin,d(i))
          dmax=max(dmax,d(i))
        else
          d(i) = -999.0
        endif
      enddo
c
      return
      end
