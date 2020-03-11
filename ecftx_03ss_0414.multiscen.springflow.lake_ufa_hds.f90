!============================================================================
!   ecftx_03ss_0414.multiscen.springflow.lake_ufa_hds.f90
!
!   This code is a combination of the original codes:
!   - ecftx_03ss_0414.lake_ufa_hds.f
!   - ecftx_03ss_0414.springflow.f
!   WEI JIN,    Wednesday, 4/24/2019
!
!   This code is written specifically for the ECFTX model,
!   version 03ss_0414.
!
!   PROGRAM FUNCTIONS:
!   The program performs the following functions for each heads
!   file that the program is instructed to process:
!   1. Read and extract springflow and lake UFA-heads
!   2. Calculate the average lake head levels for each lake
!   3. Calculate each springs' springflow from the head,
!      pool elevation, and conductance
!
!   WHAT THE PROGRAM EXPECTS:
!   The program expects an input control file where each line
!   defines the file names of input and output files associated
!   with a single heads file needing to be processed. Each line
!   of the input control file to be processed MUST have the following
!   columns in the order listed:
!   1     LINE_NUMBER - integer number for user reference
!   2     STRESS-PERIOD_NAME_FILE - input file listing the Stress Periods
!   3     BINARY_HEAD_FILE - input BINARY heads file
!   4     SORTED_LAKE_CELL_FILE - input file listing sorted lake cells
!   5     SORTED_LAKE_ID_AND_CELL_COUNTS - input file listing sorted lake id and cell counts
!   6     UFA-LAKE_HYDROGRAPH_OUTPUT_FILE_NAME - output file to write out the averaged UFA lake heads
!   7     SPRING_DRN_FILE_NAME - input drain file used for the particular model
!   8     SPRINGFLOW_OUTPUT_FILE_NAME - output file to write out springflows
!
!   The input control file MAY contain blank or commented-out lines.
!   Full or relative PATHS of files are accepted.
!   Example input control file:
!
!   ----------------------------------------
!   # 1     LINE_NUMBER
!   # 2     STRESS-PERIOD_NAME_FILE
!   # 3     BINARY_HEAD_FILE
!   # 4     SORTED_LAKE_CELL_FILE
!   # 5     SORTED_LAKE_ID_AND_CELL_COUNTS
!   # 6     UFA-LAKE_HYDROGRAPH_OUTPUT_FILE_NAME
!   # 7     SPRING_DRN_FILE_NAME
!   # 8     SPRINGFLOW_OUTPUT_FILE_NAME
!
!   1 "../stress_periods.txt" TR11.hds "../sorted_lake_cells.txt" "../sorted_lake_id_cell_count.txt" output.lake_ufa_hds.dat "../20190807.drn" output.springflow.dat
!   2 "stress_periods.txt" "SR_hds_files/SR136.hds" "lake_cells.txt" "sorted_cell_count.txt" "SR_dat_files/lake_ufa_hds/SR136.lake_ufa_hds_out.dat" "20190730.drn" "SR_dat_files/springflow/SR136.springflow_out.dat"
!   # 3 "../stress_periods.txt" TR11.hds "../sorted_lake_cells.txt" "../sorted_cell_count.txt" output.lake_ufa_hds.dat "20190807.drn" output.springflow.dat
!   <a blank line>
!   <a blank line>
!   # Just a little comment line
!   4 "stress_periods_2.txt" "hds_files/SR136.hds" "lake_cells.txt" "lake_id_and_cell_count.txt" "lake_ufa_hds/SR136.lake_ufa_hds_out.dat" "20190730.drn" "springs/SR136.springflow_out.dat"
!   ----------------------------------------
!
!   USAGE:
!   Names within <> should be replaced with the correct names.
!
!   <PATH>/ecftx_03ss_0414.multiscen.springflow.lake_ufa_hds.exe <input_control_file> -v[verbose print, optional, default is off]
!
!   Example of how to run this program with simple stdout print:
!   ./ecftx_03ss_0414.multiscen.springflow.lake_ufa_hds.exe input_control.in
!
!   Example of how to run this program with verbose stdout print:
!   ./ecftx_03ss_0414.multiscen.springflow.lake_ufa_hds.exe input_control.in -v
!
!
!
!   COMPILE WITH:
!   gfortran -g ecftx_03ss_0414.multiscen.springflow.lake_ufa_hds.f90 -o ecftx_03ss_0414.multiscen.springflow.lake_ufa_hds.exe
!
!   Written 20190924. PMBremner
!
!   Modified 20191014. PMBremner:
!       - Added ability to comment out lines in the input control file,
!           as well as add blank lines
!       - Added more notes in the comment header
!       - Added Usage notes that output to stdout when no arguments given
!       - Added option to print out all information to stdout, or just each
!           line of the input control file
!============================================================================

PROGRAM MAIN

! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    ! Import intrinsic functions
    use, intrinsic :: iso_fortran_env


    implicit none

    ! General variables
    !--------------------------
    ! Define global precision
    integer, parameter :: SP = KIND(1.0) ! Fortran 90
    integer, parameter :: DP = KIND(1.0D0) ! Fortran 90 !*** this precision seems to cause issues, memory maybe?
    !integer, parameter :: p8 = selected_real_kind(8,50) ! Fortran 95 and later

    integer, parameter :: unit_contfle=10
    integer, parameter :: unit_SP = 11, unit_LCF = 12, unit_HDS = 14
    integer, parameter :: unit_L_ID = 15, unit_UFA_L = 16, unit_SPR_DRN = 17, unit_SPROUT = 18
    integer :: ioerr
    integer :: narg, arg

	integer, parameter :: NROW=603, NCOL=740, NLAY=11, NSP=133, NSTEP=1, NCBC=12
	integer :: line_number
	integer :: IT1, IT2, NC, NR, IL
	integer :: ISP, I, ISTEP, ILAY, ICELL, IROW, ICOL

    real(SP) :: PERTIM, TOTIM

	character(len=16) :: TEXT1

	character(len=500) :: HDS_FILE, STRESS_PERIOD_FILE
	character(len=6) :: STRESS_PERIOD(NSP)

	character(len=100) :: CONTROL_FILE !="2040_refPF.lake_spring_testfile_PMB.in"
	character(len=500) :: input_line(8), input_line2
	character(len=5) :: trial_read
	character(len=1) :: tmp1, tmp2, tmp3, tmp4
    character(len=1), parameter :: commentchar="#", blankchar=" "
    character(len=2) :: OPARG_2

    logical :: verbose_print
    !--------------------------


    ! LAKES Specific Variables
    !--------------------------
!	PARAMETER(NUM_LAKES = 483, NUM_LAKE_CELLS = 18044)
!	PARAMETER(NUM_LAKES = 484, NUM_LAKE_CELLS = 18048) !add Lake Hodge, 4 cells
	integer, parameter :: NUM_LAKES = 488, NUM_LAKE_CELLS = 18180 !add splitted lucy and minneola, 132 cells
	integer :: LAKE_ID, NUM_CELL_COUNT, ILAKE  ! LAKES
	integer :: IROW_LAKES(NUM_LAKE_CELLS), ICOL_LAKES(NUM_LAKE_CELLS)  ! LAKES


    real(SP) :: HDS_LAKE(NUM_LAKE_CELLS,NSP)  ! LAKES
	real(SP) :: AVERAGE_HDS_LAKE(NUM_LAKES,NSP)  ! LAKES

    character(len=500) :: UFA_LAKE_HDS_FILE, LAKE_CELL_FILE, LAKE_ID_FILE  ! LAKES
	character(len=50) :: LAKE_NAME  ! LAKES
	!--------------------------


	! SPRINGS Specific Variables
    !--------------------------
    integer :: ISPRING, NSPRINGS  ! SPRINGS
	integer :: SPRINGKEY  ! SPRINGS

	real(SP) :: HDS(NCOL,NROW,NLAY)

    real(SP) :: SPRINGFLOW  ! SPRINGS
    real(SP) :: PELEV, COND  ! SPRINGS

    character(len=500) :: SPRING_DRN_FILE, SPRNGFLOW_OUT_FILE  ! SPRINGS
    !character*20 :: SPRINGNAME(6)  ! SPRINGS
    character(len=20) :: SPRINGNAME(6)  ! SPRINGS
    !--------------------------

! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

!   Initialize the output to stdout to not be Verbose
    verbose_print = .false.



!   GET THE NAME OF THE INPUT CONTROL FILE FROM THE ARGUMENT
	! Get the commmand-line arguments
	narg=command_argument_count()

	! Check that the number of arguments is correct
	if (narg .NE. 1 .and. narg .NE. 2) then
        print*, ""
        print*, "PROGRAM FUNCTIONS:"
        print*, "The program performs the following functions for each heads"
        print*, "file that the program is instructed to process:"
        print*, "1. Read and extract springflow and lake UFA-heads"
        print*, "2. Calculate the average lake head levels for each lake"
        print*, "3. Calculate each springs' springflow from the head,"
        print*, "   pool elevation, and conductance"
        print*, ""
        print*, "WHAT THE PROGRAM EXPECTS:"
        print*, "The program expects an input control file where each line"
        print*, "defines the file names of input and output files associated"
        print*, "with a single heads file needing to be processed. Each line"
        print*, "of the input control file to be processed MUST have the following"
        print*, "columns in the order listed:"
        print*, "1     LINE_NUMBER - integer number for user reference"
        print*, "2     STRESS-PERIOD_NAME_FILE - input file listing the Stress Periods"
        print*, "3     BINARY_HEAD_FILE - input BINARY heads file"
        print*, "4     SORTED_LAKE_CELL_FILE - input file listing sorted lake cells"
        print*, "5     SORTED_LAKE_ID_AND_CELL_COUNTS - input file listing sorted lake id and cell counts"
        print*, "6     UFA-LAKE_HYDROGRAPH_OUTPUT_FILE_NAME - output file to write out the averaged UFA lake heads"
        print*, "7     SPRING_DRN_FILE_NAME - input drain file used for the particular model"
        print*, "8     SPRINGFLOW_OUTPUT_FILE_NAME - output file to write out springflows"
        print*, ""
        print*, "The input control file MAY contain blank or commented-out lines."
        print*, "Full or relative PATHS of files are accepted."
        print*, "Example input control file:"
        print*, ""
        print*, "----------------------------------------"
        print*, "# 1     LINE_NUMBER"
        print*, "# 2     STRESS-PERIOD_NAME_FILE"
        print*, "# 3     BINARY_HEAD_FILE"
        print*, "# 4     SORTED_LAKE_CELL_FILE"
        print*, "# 5     SORTED_LAKE_ID_AND_CELL_COUNTS"
        print*, "# 6     UFA-LAKE_HYDROGRAPH_OUTPUT_FILE_NAME"
        print*, "# 7     SPRING_DRN_FILE_NAME"
        print*, "# 8     SPRINGFLOW_OUTPUT_FILE_NAME"
        print*, ""
        print*, '1 "../sp1.txt" hds1.hds cells_1.txt cell_count_1.txt out.lake_1.dat drain1.drn out.springflow_1.dat'
        print*, '2 sp2.txt hds2.hds cells_2.txt cell_count_2.txt "lakes/out.lake_2.dat" drain2.drn "sprgs/out.springflow_2.dat"'
        print*, '# 3 "../sp3.txt" hds3.hds cells_3.txt cell_count_3.txt out.lake_3.dat drain3.drn out.springflow_3.dat'
        print*, "<a blank line>"
        print*, "<a blank line>"
        print*, "# Just a little comment line"
        print*, "4 sp4.txt hds4.hds cells_4.txt cell_count_4.txt out.lake_4.dat drain4.drn out.springflow_4.dat"
        print*, "----------------------------------------"
        print*, ""
        print*, "USAGE:"
        print*, "Names within <> should be replaced with the correct names."
        print*, ""
        print*, "<PATH>/ecftx_03ss_0414.multiscen.springflow.lake_ufa_hds.exe", &
                "<input_control_file> -v[verbose print, optional, default is off]"
        print*, ""
        print*, "Example of how to run this program with simple stdout print:"
        print*, "./ecftx_03ss_0414.multiscen.springflow.lake_ufa_hds.exe input_control.in"
        print*, ""
        print*, "Example of how to run this program with verbose stdout print:"
        print*, "./ecftx_03ss_0414.multiscen.springflow.lake_ufa_hds.exe input_control.in -v"
        print*, ""
        stop
	endif

	! Get the input file
	arg=1
	call get_command_argument(arg,CONTROL_FILE)

	! Check if the verbose flag was input
	if (narg .EQ. 2) then
        arg=2
        call get_command_argument(arg,OPARG_2)

        if (OPARG_2 .EQ. "-v") then
            verbose_print = .true.
        endif
    endif


!   =======================================================
!   READ THE MASTER CONTROL FILE
!
!   This file is read in a do loop until the end of
!   file is reached.
!
!   Header describes the column format:
!   # 1     LINE_NUMBER
!   # 2     STRESS-PERIOD_NAME_FILE
!   # 3     BINARY_HEAD_FILE
!   # 4     SORTED_LAKE_CELL_FILE
!   # 5     SORTED_LAKE_ID_AND_CELL_COUNTS
!   # 6     UFA-LAKE_HYDROGRAPH_FILE_NAME
!   # 7     SPRING_DRN_FILE_NAME
!   # 8     SPRINGFLOW_OUTPUT_FILE_NAME
!   =======================================================

open(unit=unit_contfle, file=CONTROL_FILE, form='FORMATTED', status='OLD', iostat=ioerr &
    , action='read', position='rewind',access='sequential')
if (ioerr .ne. 0) then
    print*, "ERROR: Problem opening: ",CONTROL_FILE
    stop
endif

!! Skip the header lines
!do I=1,9
!    read(unit_contfle,*)
!enddo


! Read through the rest of the file
inputloop: do

    ! Read in a small part of the line to check for blank or commented lines
    read(unit_contfle,*, iostat=ioerr) trial_read
    if (ioerr .gt. 0) then
        print*, "ERROR: Problem reading the line containing: ",trial_read
        stop
    elseif (ioerr .eq. iostat_end) then
        ! End of File...Exit the loop
        exit inputloop
    endif


    ! Insert a blank line
    print*, ""


    ! Look for the comment character
    ! If the comment character is detected, cycle to the next loop iteration
    !if ( index (input_line(1), commentchar) .ne. 0 .and. index (input_line(1), blankchar) .ne. 0 ) cycle inputloop
    !if ( index (input_line2, commentchar) .ne. 0 ) cycle inputloop
    !if ( index (input_line(1), commentchar) .ne. 0 ) cycle inputloop
    !if ( index (tmp1, commentchar) .ne. 0 ) cycle inputloop
    if ( index (trial_read, commentchar) .ne. 0 .and. index (trial_read, blankchar) .ne. 0 ) then
        !print*, trial_read

        ! Move to the next loop increment
        cycle inputloop
    else
        !print*, "going back one space"

        ! Rewind the file record one space
        backspace (unit_contfle)

        !print*, "reading the line again"

        read(unit_contfle,*,iostat=ioerr) (input_line(i), i=1,8)
        !read(unit_contfle,'(A)',iostat=ioerr) (input_line(i), i=1,8)
        !read(unit_contfle,'(A)',iostat=ioerr) input_line2
        if (ioerr .gt. 0) then
            print*, "ERROR: Problem reading the line containing: ",input_line(1)," ", input_line(2)
            !print*, "ERROR: Problem reading the line containing: ",input_line2
            stop
        elseif (ioerr .eq. iostat_end) then
            ! End of File...Exit the loop
            exit inputloop
        endif

        !print*, (trim(adjustl(input_line(i))), i=1,8)
    endif


    !print*, (input_line(i), i=1,8)
    !tmp1 = trim(adjustl(input_line(1)))
    !tmp2 = trim(adjustl(input_line(2)))
    !tmp3 = trim(adjustl(input_line(3)))
    !tmp4 = trim(adjustl(input_line(4)))
    !print*, tmp1, tmp2, tmp3, tmp4


    ! No comment character detected ... moving forward
    !print*, (input_line(i), i=1,8)


    ! Put line components into their correctly typed variables
    read(input_line(1), *) line_number
    read(input_line(2), '(A)') STRESS_PERIOD_FILE
    read(input_line(3), '(A)') HDS_FILE
    read(input_line(4), '(A)') LAKE_CELL_FILE
    read(input_line(5), '(A)') LAKE_ID_FILE
    read(input_line(6), '(A)') UFA_LAKE_HDS_FILE
    read(input_line(7), '(A)') SPRING_DRN_FILE
    read(input_line(8), '(A)') SPRNGFLOW_OUT_FILE


    print*, line_number
    print*, trim(adjustl(STRESS_PERIOD_FILE))
    print*, trim(adjustl(HDS_FILE))
    print*, trim(adjustl(LAKE_CELL_FILE))
    print*, trim(adjustl(LAKE_ID_FILE))
    print*, trim(adjustl(UFA_LAKE_HDS_FILE))
    print*, trim(adjustl(SPRING_DRN_FILE))
    print*, trim(adjustl(SPRNGFLOW_OUT_FILE))

    !print*, line_number, " ", trim(adjustl(STRESS_PERIOD_FILE)), " " &
    !        , trim(adjustl(HDS_FILE)), " ", trim(adjustl(LAKE_CELL_FILE)), " " &
    !        , trim(adjustl(LAKE_ID_FILE)), " ", trim(adjustl(UFA_LAKE_HDS_FILE)), " " &
    !        , trim(adjustl(SPRING_DRN_FILE)), " ", trim(adjustl(SPRNGFLOW_OUT_FILE))
    print*, ""

!   Old version of reading in the control input file: does not check for comment lines
!    ! Read in the filenames
!    read(unit_contfle,*, iostat=ioerr)    line_number &
!                                        , STRESS_PERIOD_FILE &
!                                        , HDS_FILE &
!                                        , LAKE_CELL_FILE &
!                                        , LAKE_ID_FILE &
!                                        , UFA_LAKE_HDS_FILE &
!                                        , SPRING_DRN_FILE &
!                                        , SPRNGFLOW_OUT_FILE
!    if (ioerr .gt. 0) then
!        print*, "ERROR: Problem reading line number: ",line_number
!        stop
!    elseif (ioerr .lt. 0) then
!        ! End of File...Exit the loop
!        exit inputloop
!    endif


!   =======================================================
!   READ THE STRESS PERIOD FILE
!   =======================================================
	open(unit=unit_SP, file=trim(STRESS_PERIOD_FILE), form='FORMATTED', status='OLD', iostat=ioerr &
        , action='read', position='rewind', access='sequential')
    if (ioerr .ne. 0) then
        print*, "ERROR: Problem opening: ",STRESS_PERIOD_FILE
        stop
    endif

	do ISP=1,NSP
        read(unit_SP,*, iostat=ioerr) STRESS_PERIOD(ISP)
        if (ioerr .ne. 0) then
            print*, "ERROR: Problem reading Stress Period: ",ISP
            stop
        endif

        if (verbose_print) write(*,*) STRESS_PERIOD(ISP)
	enddo

	close(unit_SP)
!   -------------------------------------------------------


! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!   LAKE HEADS AND SPRINGFLOW SECTION
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


!   =======================================================
!   READ THE SORTED LAKE CELL FILE
!   =======================================================
	open(unit=unit_LCF, file=LAKE_CELL_FILE, form='FORMATTED', status='OLD', iostat=ioerr &
        , action='read', position='rewind', access='sequential')
	if (ioerr .ne. 0) then
        print*, "ERROR: Problem opening: ",LAKE_CELL_FILE
        stop
    endif

	read(unit_LCF,*)
	do I=1,NUM_LAKE_CELLS
        read(unit_LCF,*, iostat=ioerr) IROW_LAKES(I), ICOL_LAKES(I)
        if (ioerr .ne. 0) then
            print*, "ERROR: Problem reading Sorted Lake Cell number: ",I
            stop
        endif
	enddo

	close(unit_LCF)
!   -------------------------------------------------------


!	=======================================================
!	OPEN AND READ SPRING DRN FILE
!
!	OPEN AND READ THE BINARY HEADS FILE
!
!	OPEN AND WRITE TO SPRINGFLOW OUTPUT FILE NAME
!	=======================================================

    ! Spring Drain file
    open(unit=unit_SPR_DRN, file=SPRING_DRN_FILE, form='FORMATTED', status='OLD', iostat=ioerr &
        , action='read', position='rewind', access='sequential')
    if (ioerr .ne. 0) then
        print*, "ERROR: Problem opening: ",SPRING_DRN_FILE
        stop
    endif

	! Skip 3 header lines in Spring Drain file
	read(unit_SPR_DRN,*)
	read(unit_SPR_DRN,*)
	read(unit_SPR_DRN,*)


    ! Heads file
    open(unit=unit_HDS, file=TRIM(HDS_FILE), form='UNFORMATTED', status='OLD', iostat=ioerr &
        , action='read', position='rewind', access='stream')
	if (ioerr .ne. 0) then
        print*, "ERROR: Problem opening: ",HDS_FILE
        stop
    endif


    ! Springflow Output file
    open(unit=unit_SPROUT, file=SPRNGFLOW_OUT_FILE, form='FORMATTED', status='REPLACE', iostat=ioerr &
        , action='write', position='rewind', access='sequential')
	if (ioerr .ne. 0) then
        print*, "ERROR: Problem opening: ",SPRNGFLOW_OUT_FILE
        stop
    endif

	write(unit_SPROUT,'(10A20)')  'MONTH_YEAR' &
                                , 'STRESS_PERIOD' &
                                , 'LAYER' &
                                , 'ROW' &
                                , 'COL' &
                                , 'POOL_ELEVATION' &
                                , 'CONDUCTANCE' &
                                , 'SPRINGKEY' &
                                , 'SPRINGFLOW_CFD' &
                                , 'SPRINGFLOW_CFS'


    do ISP =1,NSP

        do ILAY=1,NLAY
            read(unit_HDS, iostat=ioerr) IT1,IT2,PERTIM,TOTIM,TEXT1,NC,NR,IL
            if (ioerr .ne. 0) then
                print*, "ERROR: Problem reading the HDS header line: ",HDS_FILE
                stop
            endif

            if (verbose_print) write(*,*) IT1,IT2,PERTIM,TOTIM,TEXT1,NC,NR,IL

            read(unit_HDS, iostat=ioerr) HDS(:,:,ILAY)
            if (ioerr .ne. 0) then
                print*, "ERROR: Problem reading the HDS vector: ",HDS_FILE
                stop
            endif

            !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            ! FOR LAKES SECTION
            !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            if (ILAY .EQ. 3) then
                do I=1,NUM_LAKE_CELLS
                    HDS_LAKE(I,ISP) = HDS(ICOL_LAKES(I),IROW_LAKES(I),ILAY)
                    !HDS_LAKE(I,ISP) = XVS(ICOL_LAKES(I),IROW_LAKES(I))
                    !	WRITE(*,*)ISP,I,ICOL_LAKES(I),IROW_LAKES(I),HDS_LAKE(I,ISP)
                enddo
            endif
            !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        enddo

        read(unit_SPR_DRN,*, iostat=ioerr) NSPRINGS
        if (ioerr .ne. 0) then
            print*, "ERROR: Problem reading the number of springs: ",SPRNGFLOW_OUT_FILE
            stop
        endif

        do ISPRING=1,NSPRINGS
            read(unit_SPR_DRN,*, iostat=ioerr) ILAY,IROW,ICOL,PELEV,COND,SPRINGKEY
            if (ioerr .ne. 0) then
                print*, "ERROR: Problem reading the Spring Drain File, line: ",ISPRING
                stop
            endif

            ! Account for rounding to zero from the negative side
            ! *** this behavior changed after Fortran 77 ***
            if (PELEV .eq. -0.0) then
                PELEV = 0.0
            endif


            SPRINGFLOW = AMAX1(0.,(HDS(ICOL,IROW,ILAY)-PELEV))*COND

            !write(unit_SPROUT,'(A20,4I20,F20.2,F20.0,I20,F20.0,F20.2)')
            !write(unit_SPROUT,'(A20,4I20,F20.2,F20.0,I20,F20.0,F20.7)')   TRIM(STRESS_PERIOD(ISP)) &
            !                                                            , ISP,ILAY,IROW,ICOL,PELEV,COND,SPRINGKEY &
            !                                                            !    ,	(TRIM(SPRINGNAME(I)),I=1,2) &
            !                                                            , SPRINGFLOW,SPRINGFLOW/3600./24.
            write(unit_SPROUT,*) TRIM(STRESS_PERIOD(ISP)) &
                                 , ISP,ILAY,IROW,ICOL,PELEV,COND,SPRINGKEY &
                                 !    ,  (TRIM(SPRINGNAME(I)),I=1,2) &
                                 , SPRINGFLOW,SPRINGFLOW/3600./24.

        enddo
    enddo

    close(unit_SPR_DRN)
    close(unit_SPROUT)
    close(unit_HDS)
!   -------------------------------------------------------


!   =======================================================
!   OPEN AND READ THE SORTED LAKE ID AND CELL COUNTS
!
!   WRITE TO THE OUTPUT FILE
!   =======================================================
	open(unit=unit_L_ID, file=LAKE_ID_FILE, form='FORMATTED', status='OLD', iostat=ioerr &
        , action='read', position='rewind', access='sequential')
    if (ioerr .ne. 0) then
        print*, "ERROR: Problem opening: ",LAKE_ID_FILE
        stop
    endif

	read(unit_L_ID,*)  ! Skip line


    ! Open the output file
	open(unit=unit_UFA_L, file=UFA_LAKE_HDS_FILE, form='FORMATTED', status='REPLACE', iostat=ioerr &
        , action='write', position='rewind', access='sequential')
	if (ioerr .ne. 0) then
        print*, "ERROR: Problem opening: ",UFA_LAKE_HDS_FILE
        stop
    endif

    ! Write to file the Lake Id and Name followed by Stress Period values
	!write(unit_UFA_L,'(A20,A50,133A20)') 'LAKE_ID','LAKE_NAME',(STRESS_PERIOD(ISP),ISP=1,NSP)
	write(unit_UFA_L,'(A20,A50,133A20)') 'LAKE_ID',(STRESS_PERIOD(ISP),ISP=1,NSP)

	I = 0
	AVERAGE_HDS_LAKE = 0.

    do ILAKE=1,NUM_LAKES

        read(unit_L_ID,*,  iostat=ioerr) LAKE_ID,LAKE_NAME,NUM_CELL_COUNT
        if (ioerr .ne. 0) then
            print*, "ERROR: Problem reading the LAKE info on line: ",ILAKE
            stop
        endif

        if (verbose_print) write(*,*) LAKE_ID,LAKE_NAME,NUM_CELL_COUNT

        do ICELL=1,NUM_CELL_COUNT
            I = I + 1
            AVERAGE_HDS_LAKE(ILAKE,:) = AVERAGE_HDS_LAKE(ILAKE,:) + HDS_LAKE(I,:)
        enddo

        AVERAGE_HDS_LAKE = AVERAGE_HDS_LAKE/DFLOAT(NUM_CELL_COUNT)

        ! Write to file the Lake Id and Name followed by Average Head values for each Stress Period
        !	WRITE(*,*)ILAKE,AVERAGE_HDS_LAKE(ILAKE,1:3)
        !write(unit_UFA_L,'(I20,A50,133F20.5)') LAKE_ID,TRIM(LAKE_NAME), AVERAGE_HDS_LAKE(ILAKE,1:NSP)
        !write(unit_UFA_L,*) LAKE_ID,TRIM(LAKE_NAME), AVERAGE_HDS_LAKE(ILAKE,1:NSP)
        write(unit_UFA_L,*) LAKE_ID, AVERAGE_HDS_LAKE(ILAKE,1:NSP)
    enddo

	close(unit_L_ID)
	close(unit_UFA_L)

! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

enddo inputloop

close(unit_contfle)
!   -------------------------------------------------------


!============================================================================
END PROGRAM MAIN
