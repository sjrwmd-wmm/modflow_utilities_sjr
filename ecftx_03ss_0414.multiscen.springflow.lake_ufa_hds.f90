!============================================================================
!   lake_head_and_springflow_extraction.f90
!
!   This code is a modified combination of the original codes:
!   - ecftx_03ss_0414.lake_ufa_hds.f
!   - ecftx_03ss_0414.springflow.f
!   written by WEI JIN at St Johns River Water Management District, Wednesday, 4/24/2019
!
!   This code was originally written specifically for the ECFTX model,
!   version 03ss_0414. But was generalized to work for other groundwater models.
!
!   -------  PROGRAM OVERVIEW:
!   This program is a post-processing tool to output heads and
!   springflow information for a given groundwater model.
!   For a given MODFLOW heads file, the program performs
!   the following functions:
!       1. Read and extract springflow and lake heads values
!       2. Calculate the average lake head levels for each lake
!       3. Calculate each springs' springflow from the head,
!          pool elevation, and conductance
!       4. Output lake heads and springflows to file
!
!   The program is able to post-process multiple, unrelated
!   models and scenarios in a single run. Also, the model layer
!   used to extract lake heads is user defined.
!
!
!   The full documentation is available by running this code with:
!       ./lake_head_and_springflow_extraction.exe --help
!   OR
!       ./lake_head_and_springflow_extraction.exe -h
!
!
!
!   COMPILE WITH:
!   gfortran -g lake_head_and_springflow_extraction.f90 -o bin/lake_head_and_springflow_extraction.exe
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
!
!   Modified 20200317. PMBremner:
!       - Implemented a MODEL_PARAMETER_FILE to set model array sizes and number of lakes
!
!   Modified 20200401. PMBremner:
!       - Fixed documentation
!       - Changed name of the code to reflect its new generalized model functionality
!         original name ecftx_03ss_0414.multiscen.springflow.lake_ufa_hds.f90
!============================================================================


!############################################################################
!
PROGRAM MAIN
!
!############################################################################
    
    
    ! xoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxox
    !
    !               SET HEADERS AND VARIABLES
    !
    ! xoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxox
    
    ! Import intrinsic functions
    use, intrinsic :: iso_fortran_env
    
    
    implicit none
    
    ! General variables
    !--------------------------
    ! Define global precision
    integer, parameter :: SP = KIND(1.0) ! Fortran 90
    integer, parameter :: DP = KIND(1.0D0) ! Fortran 90 !*** this precision seems to cause issues, memory maybe?
    !integer, parameter :: p8 = selected_real_kind(8,50) ! Fortran 95 and later
    
    integer, parameter :: unit_contfle=10, unit_hdr_file=19
    integer, parameter :: unit_SP = 11, unit_LCF = 12, unit_HDS = 14
    integer, parameter :: unit_L_ID = 15, unit_UFA_L = 16, unit_SPR_DRN = 17, unit_SPROUT = 18
    integer :: ioerr
        
    integer :: narg, arg
    character(len=100) :: TMP_ARG
    character(len=1) :: dash_check
    logical :: verbose_print, full_doc_flag, usage_doc_flag, skip_iteration
    integer :: j
    
    ! Input file ! Example: "2040_refPF.lake_spring_testfile_PMB.in"
    character(len=100) :: CONTROL_FILE_TMP
    character(:),allocatable :: CONTROL_FILE
    
    integer, parameter :: number_of_input_data_coloumns = 9  !!! PMB 20200316. Move to the header control file?
    character(len=500) :: input_line(number_of_input_data_coloumns), input_line2
    character(len=5) :: trial_read
    character(len=1) :: tmp1, tmp2, tmp3, tmp4
    character(len=1), parameter :: commentchar="#", blankchar=" "
    
    integer :: n_progfunc_doc, n_infiles_doc, n_outfiles_doc
    integer :: n_usage_doc, n_compile_doc, n_author_doc
    character(len=150),dimension(:), allocatable :: progfunc_doc, infiles_doc, outfiles_doc
    character(len=150),dimension(:), allocatable :: usage_doc, compile_doc, author_doc
    
    
    ! Values read from header control file
    !integer, parameter :: NROW=603, NCOL=740, NLAY=11, NSP=133, NSTEP=1, NCBC=12
    integer :: NROW, NCOL, NLAY, NSP, NSTEP, NCBC
    
    integer :: line_number
    integer :: IT1, IT2, NC, NR, IL
    integer :: ISP, I, ISTEP, ILAY, ICELL, IROW, ICOL
    
    real(SP) :: PERTIM, TOTIM
    
    character(len=16) :: TEXT1
    
    character(len=100) :: HEADER_CONTROL_FILE !="header_control_file.dat"
    integer :: input_header_value
    character(len=3) :: keyword1
    character(len=30) :: keyword2

    character(len=500) :: HDS_FILE, STRESS_PERIOD_FILE
    !character(len=6) :: STRESS_PERIOD(NSP)
    character(len=6), dimension(:), allocatable :: STRESS_PERIOD
    !--------------------------


    ! LAKES Specific Variables
    !--------------------------
    !	PARAMETER(NUM_LAKES = 483, NUM_LAKE_CELLS = 18044)
    !	PARAMETER(NUM_LAKES = 484, NUM_LAKE_CELLS = 18048) !add Lake Hodge, 4 cells
    !integer, parameter :: NUM_LAKES = 488, NUM_LAKE_CELLS = 18180 !add splitted lucy and minneola, 132 cells
    
    ! Values read from header control file
    integer :: NUM_LAKES, NUM_LAKE_CELLS
    integer :: LAKE_HDS_LAYER
    
    
    integer :: LAKE_ID, NUM_CELL_COUNT, ILAKE  ! LAKES
    !integer :: IROW_LAKES(NUM_LAKE_CELLS), ICOL_LAKES(NUM_LAKE_CELLS)  ! LAKES
    integer, dimension(:), allocatable :: IROW_LAKES, ICOL_LAKES


    !real(SP) :: HDS_LAKE(NUM_LAKE_CELLS,NSP)  ! LAKES
    !real(SP) :: AVERAGE_HDS_LAKE(NUM_LAKES,NSP)  ! LAKES
    real(SP), dimension(:,:), allocatable :: HDS_LAKE
    real(SP), dimension(:,:), allocatable :: AVERAGE_HDS_LAKE

    character(len=500) :: UFA_LAKE_HDS_FILE, LAKE_CELL_FILE, LAKE_ID_FILE  ! LAKES
    character(len=50) :: LAKE_NAME  ! LAKES
    !--------------------------


    ! SPRINGS Specific Variables
    !--------------------------
    integer :: ISPRING, NSPRINGS  ! SPRINGS
    integer :: SPRINGKEY  ! SPRINGS

    !real(SP) :: HDS(NCOL,NROW,NLAY)
    real(SP), dimension(:,:,:), allocatable  :: HDS

    real(SP) :: SPRINGFLOW  ! SPRINGS
    real(SP) :: PELEV, COND  ! SPRINGS

    character(len=500) :: SPRING_DRN_FILE, SPRNGFLOW_OUT_FILE  ! SPRINGS
    !character*20 :: SPRINGNAME(6)  ! SPRINGS
    character(len=20) :: SPRINGNAME(6)  ! SPRINGS   !!! PMB 20200316. Currently hardcoded but unused, may need changed
    !--------------------------
    
    ! ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
    !
    !               END HEADERS AND VARIABLES
    !
    ! ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
    
    
    
    ! xoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxox
    !
    ! PROCESS COMMAND-LINE ARGUEMENTS
    !
    ! xoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxox
    
    
    ! Retrieve the number of command-line arguments
    narg = command_argument_count()
    
    
    ! ===========================================
    ! Check for flags and input file
    ! ===========================================
    
    ! Initialize all the command-line option flags
    full_doc_flag = .false.
    usage_doc_flag = .false.
    verbose_print = .false. ! not verbose print statements
    skip_iteration = .false.
    
    if (narg .LT. 1) then
        usage_doc_flag = .true.
    else
        do arg=1,narg
            if (skip_iteration) then
                skip_iteration = .false.
                CYCLE ! skip the next iteration
            else
                call get_command_argument(arg,TMP_ARG)
                if (TMP_ARG .EQ. "-v") then
                    verbose_print = .true.
                else if (TMP_ARG .EQ. "-in") then
                    if (narg .LT. (arg+1)) then
                        usage_doc_flag = .true.
                        print*, ""
                        print*, "ERROR: No Input Filename provided after -in!"
                        print*, ""
                        EXIT
                    else
                        call get_command_argument((arg+1),CONTROL_FILE_TMP)
                        write(dash_check,'(A1)') CONTROL_FILE_TMP
                        if (dash_check .EQ. "-") then
                            usage_doc_flag = .true.
                            print*, ""
                            print*, "ERROR: No input filename provided after -in!"
                            print*, ""
                            print*, 'It is possible that the Input Filename begins with a dash ("-").'
                            print*, "A leading dash is reserved for command-line options. Ensure that"
                            print*, "the Input Filename provided does not begin with this character."
                            print*, ""
                            print*, ""
                            EXIT
                        else
                            skip_iteration = .true. ! skip the next iteration
                            allocate (character(len=len_trim(CONTROL_FILE_TMP)) :: CONTROL_FILE)
                            CONTROL_FILE = trim(adjustl(CONTROL_FILE_TMP))
                        endif
                    endif
                else if (TMP_ARG .EQ. "-h") then
                    full_doc_flag = .true.
                    EXIT
                else if (TMP_ARG .EQ. "--help") then
                    full_doc_flag = .true.
                    EXIT
                else
                    usage_doc_flag = .true.
                endif
            endif
        enddo
    endif
    ! -------------------------------------------
    
    
    
    ! ===========================================
    ! DOCUMENTATION
    ! ===========================================
    
    if (usage_doc_flag .or. full_doc_flag) then
	
	n_progfunc_doc = 20
        allocate (progfunc_doc(n_progfunc_doc))
        progfunc_doc(1)  = ""
        progfunc_doc(2)  = ""
        progfunc_doc(3)  = "-------  NAME  -------"
        progfunc_doc(4)  = "lake_head_and_springflow_extraction"
        progfunc_doc(5)  = ""
        progfunc_doc(6)  = "-------  PROGRAM OVERVIEW  -------"
        progfunc_doc(7)  = "This program is a post-processing tool to output heads and"
        progfunc_doc(8)  = "springflow information for a given groundwater model."
        progfunc_doc(9)  = "For a given MODFLOW heads file, the program performs"
        progfunc_doc(10) = "the following functions:"
        progfunc_doc(11) = "    1. Read and extract springflow and lake heads values"
        progfunc_doc(12) = "    2. Calculate the average lake head levels for each lake"
        progfunc_doc(13) = "    3. Calculate each springs' springflow from the head,"
        progfunc_doc(14) = "       pool elevation, and conductance"
        progfunc_doc(15) = "    4. Output lake heads and springflows to file"
        progfunc_doc(16) = ""
        progfunc_doc(17) = "The program is able to post-process multiple, unrelated"
        progfunc_doc(18) = "models and scenarios in a single run. Also, the model layer"
        progfunc_doc(19) = "used to extract lake heads is user defined."
        progfunc_doc(20) = ""
        
        
        n_infiles_doc = 52
        allocate (infiles_doc(n_infiles_doc))
        infiles_doc(1)  = ""
        infiles_doc(2)  = "-------  INPUTS TO THE PROGRAM  -------"
        infiles_doc(3)  = "The program requires that the name of an input control file"
        infiles_doc(4)  = "be provided as a command-line argument. Each data line of the"
        infiles_doc(5)  = "input control file provides the program with the file names"
        infiles_doc(6)  = "and PATHs of input and output files associated"
        infiles_doc(7)  = "with a single model scenario run to be processed. Each data"
        infiles_doc(8)  = "line of the input control file to be processed MUST have the"
        infiles_doc(9)  = "following columns in the order listed:"
        infiles_doc(10) = "( [INPUT] = input file; [OUTPUT] = output file )"
        infiles_doc(11) = "1     LINE_NUMBER - integer number meant only for user reference"
        infiles_doc(12) = "2     MODEL_PARAMETER_FILE - [INPUT] file listing the model parameter values"
        infiles_doc(13) = "3     STRESS-PERIOD_NAME_FILE - [INPUT] lists the model Stress Periods"
        infiles_doc(14) = "4     BINARY_HEAD_FILE - [INPUT] binary MODFLOW heads file"
        infiles_doc(15) = "5     SORTED_LAKE_CELL_FILE - [INPUT] file listing sorted lake cells"
        infiles_doc(16) = "6     SORTED_LAKE_ID_AND_CELL_COUNTS - [INPUT] file of sorted lake id and cell counts"
        infiles_doc(17) = "7     LAKE_HEADS_OUTPUT_FILE_NAME - [OUTPUT] file of the averaged lake heads"
        infiles_doc(18) = "8     SPRING_DRN_FILE_NAME - [INPUT] drain file used for the particular model"
        infiles_doc(19) = "9     SPRINGFLOW_OUTPUT_FILE_NAME - [OUTPUT] file of the calculated springflows"
        infiles_doc(20) = ""
        infiles_doc(21) = "The input control file MAY contain blank or commented-out lines."
        infiles_doc(22) = 'Commented lines start with the "#", and are not read by the program.'
        infiles_doc(23) = "Full or relative PATHS of files are accepted."
        infiles_doc(24) = "Example input control file:"
        infiles_doc(25) = "( Note: Data lines of this file are typically long, so for this"
        infiles_doc(26) = "        example <continue> denotes the continuation of a line )"
        infiles_doc(27) = ""
        infiles_doc(28) = "----------------------------------------"
        infiles_doc(29) = "# 1     LINE_NUMBER"
        infiles_doc(30) = "# 2     MODEL PARAMETER FILE"
        infiles_doc(31) = "# 3     STRESS-PERIOD_NAME_FILE"
        infiles_doc(32) = "# 4     BINARY_HEAD_FILE"
        infiles_doc(33) = "# 5     SORTED_LAKE_CELL_FILE"
        infiles_doc(34) = "# 6     SORTED_LAKE_ID_AND_CELL_COUNTS"
        infiles_doc(35) = "# 7     LAKE_HEADS_OUTPUT_FILE_NAME"
        infiles_doc(36) = "# 8     SPRING_DRN_FILE_NAME"
        infiles_doc(37) = "# 9     SPRINGFLOW_OUTPUT_FILE_NAME"
        infiles_doc(38) = ""
        infiles_doc(39) = "# File set for model 1"
        infiles_doc(40) = '1 modparam_1.dat "../sp1.txt" hds1.hds "../sorted_lake_cells.txt"'
        infiles_doc(41) = '<continue> "../sorted_lake_id_and_cell_count.txt" "lakes/lake_hds_1.dat"'
        infiles_doc(42) = '<continue> "../drain_file.drn" "springflow/springflow_1.dat"'
        infiles_doc(43) = ""
        infiles_doc(44) = "# File set for model 2"
        infiles_doc(45) = '2 modparam_2.dat "../sp2.txt" hds2.hds "../sorted_lake_cells.txt"'
        infiles_doc(46) = '<continue> "../sorted_lake_id_and_cell_count.txt" "lakes/lake_hds_2.dat"'
        infiles_doc(47) = '<continue> "../drain_file.drn" "springflow/springflow_2.dat"'
        infiles_doc(48) = "----------------------------------------"
        infiles_doc(49) = ""
        infiles_doc(50) = "IMPORTANT: Example input and output files"
        infiles_doc(51) = "should be included in the bundle with this software."
        infiles_doc(52) = ""
        
        n_outfiles_doc = 47
        allocate (outfiles_doc(n_outfiles_doc))
        outfiles_doc(1)  = ""
        outfiles_doc(2)  = "-------  OUTPUT FILES  -------"
        outfiles_doc(3)  = "There are two output files from this program:"
        outfiles_doc(4)  = "    1  <LAKE_HEADS_OUTPUT>.dat"
        outfiles_doc(5)  = "    2  <SPRINGFLOW>.dat"
        outfiles_doc(6)  = ""
        outfiles_doc(7)  = "<LAKE_HEADS_OUTPUT>.dat is where the calculated mean"
        outfiles_doc(8)  = "lake head values are written. The heads value for each lake"
        outfiles_doc(9)  = "is extracted from a consistent, user defined model layer"
        outfiles_doc(10) = "from the MODFLOW heads file. The model cells that which"
        outfiles_doc(11) = "define a lake area are provided by the SORTED_LAKE_CELL_FILE"
        outfiles_doc(12) = "and SORTED_LAKE_ID_AND_CELL_COUNTS files. The value output"
        outfiles_doc(13) = "is the mean of all the heads values over the cells defining"
        outfiles_doc(14) = "the lake area."
        outfiles_doc(15) = ""
        outfiles_doc(16) = "The rows of <LAKE_HEADS_OUTPUT>.dat are:"
        outfiles_doc(17) = "    1    HEADER LINE DESCRIBING DATA COLUMNS"
        outfiles_doc(18) = "    2-N  OUTPUT DATA"
        outfiles_doc(19) = "The columns of <LAKE_HEADS_OUTPUT>.dat are:"
        outfiles_doc(20) = "    1    LAKE_ID"
        outfiles_doc(21) = "    2    MEAN_LAKE_HEAD (STRESS PERIOD 1)"
        outfiles_doc(22) = "    3    MEAN_LAKE_HEAD (STRESS PERIOD 2)"
        outfiles_doc(23) = "    ."
        outfiles_doc(24) = "    ."
        outfiles_doc(25) = "    ."
        outfiles_doc(26) = "    M    MEAN_LAKE_HEAD (LAST STRESS PERIOD)"
        outfiles_doc(27) = ""
        outfiles_doc(28) = "<SPRINGFLOW>.dat is where the calculated springflow value"
        outfiles_doc(29) = "for each spring listed in the MODFLOW drain file is output."
        outfiles_doc(30) = "Springflow is calculated as the max between zero and"
        outfiles_doc(31) = "[ (heads_value - pool_elevation) * spring-conductance ]."
        outfiles_doc(32) = ""
        outfiles_doc(33) = "The rows of <SPRINGFLOW>.dat are:"
        outfiles_doc(34) = "    1    HEADER LINE DESCRIBING DATA COLUMNS"
        outfiles_doc(35) = "    2-N  OUTPUT DATA"
        outfiles_doc(36) = "The columns of <SPRINGFLOW>.dat are:"
        outfiles_doc(37) = "    1     MONTH_YEAR"
        outfiles_doc(38) = "    2     STRESS_PERIOD"
        outfiles_doc(39) = "    3     MODEL LAYER"
        outfiles_doc(40) = "    4     MODEL ROW"
        outfiles_doc(41) = "    5     MODEL COLUMN"
        outfiles_doc(42) = "    6     POOL_ELEVATION"
        outfiles_doc(43) = "    7     SPRING CONDUCTANCE"
        outfiles_doc(44) = "    8     SPRINGKEY"
        outfiles_doc(45) = "    9     SPRINGFLOW_CFD"
        outfiles_doc(46) = "    10    SPRINGFLOW_CFS"
        outfiles_doc(47) = ""
        
        n_usage_doc = 26
        allocate (usage_doc(n_usage_doc))
        usage_doc(1)  = ""
        usage_doc(2)  = "-------  USAGE  -------"
        usage_doc(3)  = "Names within <> should be replaced with the correct names."
        usage_doc(4)  = ""
        usage_doc(5)  = "COMMAND SYNTAX:"
        usage_doc(6)  = "<PATH>/lake_head_and_springflow_extraction.exe <options>"
        usage_doc(7)  = ""
        usage_doc(8)  = "DESCRIPTION OF OPTIONS:"
        usage_doc(9)  = "-in <input_control_file>"
        usage_doc(10) = "-v [verbose print, optional, default is off]"
        usage_doc(11) = "-h or --help [display full documentation]"
        usage_doc(12) = ""
        usage_doc(13) = "The list of arguments can appear in any order, but -in MUST be"
        usage_doc(14) = "followed by the input filename."
        usage_doc(15) = ""
        usage_doc(16) = "Example of how to run this program with simple stdout print:"
        usage_doc(17) = "./lake_head_and_springflow_extraction.exe -in input_control.in"
        usage_doc(18) = ""
        usage_doc(19) = "Example of how to run this program with verbose stdout print:"
        usage_doc(20) = "./lake_head_and_springflow_extraction.exe -in input_control.in -v"
        usage_doc(21) = ""
        usage_doc(22) = "Example of how to display the full documentation of this program:"
        usage_doc(23) = "./lake_head_and_springflow_extraction.exe --help"
        usage_doc(24) = "OR"
        usage_doc(25) = "./lake_head_and_springflow_extraction.exe -h"
        usage_doc(26) = ""
        
        n_compile_doc = 14
        allocate (compile_doc(n_compile_doc))
        compile_doc(1)  = ""
        compile_doc(2)  = "-------  COMPILING  -------"
        compile_doc(3)  = "This program has been compiled using the GNU Fortran compiler gfortran."
        compile_doc(4)  = "No other compilers were tried, however this program was written in"
        compile_doc(5)  = "standard Fortran compliant syntax, and is expected to compile with"
        compile_doc(6)  = "no issues with any modern compiler."
        compile_doc(7)  = ""
        compile_doc(8)  = "Example using gfortran:"
        compile_doc(9)  = "    gfortran -g <source_code> -o <binary_executable>"
        compile_doc(10) = "  where"
        compile_doc(11) = "    <source_code> = lake_head_and_springflow_extraction.f90"
        compile_doc(12) = "  and"
        compile_doc(13) = "    <binary_executable> = bin/lake_head_and_springflow_extraction.exe"
        compile_doc(14) = ""
        
        n_author_doc = 14
        allocate (author_doc(n_author_doc))
        author_doc(1)  = ""
        author_doc(2)  = "-------  AUTHOR  -------"
        author_doc(3)  = "lake_head_and_springflow_extraction was originally written by Paul Bremner,"
        author_doc(4)  = "and has benefitted from contribution by others since then. The core of"
        author_doc(5)  = "this program originated from a modified combination of the codes:"
	author_doc(6)  = "    - ecftx_03ss_0414.lake_ufa_hds.f"
	author_doc(7)  = "    - ecftx_03ss_0414.springflow.f"
	author_doc(8)  = "written by Wei Jin at St Johns River Water Management District, 4/24/2019."
	author_doc(9)  = ""
	author_doc(10) = "For support or to report issues contact Paul Bremner at the"
	author_doc(11) = "St Johns River Water Management at pbremner<at>sjrwmd.com"
	author_doc(12) = ""
	author_doc(13) = "02 Apr 2020"
	author_doc(14) = ""
        ! -------------------------------------------
        
        ! ===========================================
        ! Print Documentation
        ! ===========================================
        if (full_doc_flag) then
            do j=1,n_progfunc_doc
                write(*,*) trim(progfunc_doc(j))
            enddo

            do j=1,n_infiles_doc
                write(*,*) trim(infiles_doc(j))
            enddo
            
            do j=1,n_outfiles_doc
                write(*,*) trim(outfiles_doc(j))
            enddo
            
            do j=1,n_usage_doc
                write(*,*) trim(usage_doc(j))
            enddo
            
            do j=1,n_compile_doc
                write(*,*) trim(compile_doc(j))
            enddo
            
            do j=1,n_author_doc
                write(*,*) trim(author_doc(j))
            enddo
        else if (usage_doc_flag) then
            do j=1,n_usage_doc
                write(*,*) trim(usage_doc(j))
            enddo
        endif
        
        
        ! Free Memory
        deallocate (progfunc_doc)
        deallocate (infiles_doc)
        deallocate (outfiles_doc)
        deallocate (usage_doc)
        
        ! End the program
        stop
    endif
    ! ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
    !
    ! END PROCESS COMMAND-LINE ARGUEMENTS
    !
    ! ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
    
    
    
    ! xoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxox
    !
    !   		READ THE MASTER CONTROL FILE
    !			     MARCH THROUGH LIST
    !
    !   This file is read in a do loop until the end of
    !   file is reached.
    !
    !   Header describes the column format:
    !   # 1     LINE_NUMBER
    !	# 2	HEADER CONTROL FILE
    !   # 3     STRESS-PERIOD_NAME_FILE
    !   # 4     BINARY_HEAD_FILE
    !   # 5     SORTED_LAKE_CELL_FILE
    !   # 6     SORTED_LAKE_ID_AND_CELL_COUNTS
    !   # 7     UFA-LAKE_HYDROGRAPH_FILE_NAME
    !   # 8     SPRING_DRN_FILE_NAME
    !   # 9     SPRINGFLOW_OUTPUT_FILE_NAME
    ! xoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxox

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

	    read(unit_contfle,*,iostat=ioerr) (input_line(i), i=1,number_of_input_data_coloumns)
	    !read(unit_contfle,'(A)',iostat=ioerr) (input_line(i), i=1,number_of_input_data_coloumns)
	    !read(unit_contfle,'(A)',iostat=ioerr) input_line2
	    if (ioerr .gt. 0) then
		print*, "ERROR: Problem reading the line containing: ",input_line(1)," ", input_line(2)
		!print*, "ERROR: Problem reading the line containing: ",input_line2
		stop
	    elseif (ioerr .eq. iostat_end) then
		! End of File...Exit the loop
		exit inputloop
	    endif

	    !print*, (trim(adjustl(input_line(i))), i=1,number_of_input_data_coloumns)
	endif


	!print*, (input_line(i), i=1,number_of_input_data_coloumns)
	!tmp1 = trim(adjustl(input_line(1)))
	!tmp2 = trim(adjustl(input_line(2)))
	!tmp3 = trim(adjustl(input_line(3)))
	!tmp4 = trim(adjustl(input_line(4)))
	!print*, tmp1, tmp2, tmp3, tmp4


	! No comment character detected ... moving forward
	!print*, (input_line(i), i=1,number_of_input_data_coloumns)


	! Put line components into their correctly typed variables
	! !!! PMB 20200316. Will require reconfiguring if column number becomes variable
	read(input_line(1), *) line_number
	read(input_line(2), '(A)') HEADER_CONTROL_FILE
	read(input_line(3), '(A)') STRESS_PERIOD_FILE
	read(input_line(4), '(A)') HDS_FILE
	read(input_line(5), '(A)') LAKE_CELL_FILE
	read(input_line(6), '(A)') LAKE_ID_FILE
	read(input_line(7), '(A)') UFA_LAKE_HDS_FILE
	read(input_line(8), '(A)') SPRING_DRN_FILE
	read(input_line(9), '(A)') SPRNGFLOW_OUT_FILE


	print*, line_number
	print*, trim(adjustl(HEADER_CONTROL_FILE))
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
	
	
	
	! =========================================================
	!
	! READ THE HEADER CONTROL FILE AND SET THE ARRAY SIZES
	!
	! =========================================================
	
	open(unit=unit_hdr_file, file=HEADER_CONTROL_FILE, form='FORMATTED', status='OLD', iostat=ioerr &
	, action='read', position='rewind',access='sequential')
	if (ioerr .ne. 0) then
	    print*, "ERROR: Problem opening: ",HEADER_CONTROL_FILE
	    stop
	endif
	
	! Read through the rest of the file
	headerloop: do
	    
	    ! Read in a small part of the line to check for blank or commented lines
	    read(unit_hdr_file,*, iostat=ioerr) trial_read
	    if (ioerr .gt. 0) then
		write(*,'(A,I0,A,A,A,A)') "ERROR ", ioerr, &
					  ": Problem reading the line containing: ", &
					  trial_read
		stop
	    elseif (ioerr .eq. iostat_end) then
		! End of File...Exit the loop
		exit headerloop
	    endif


	    if ( index (trial_read, commentchar) .ne. 0 .and. index (trial_read, blankchar) .ne. 0 ) then
		!print*, trial_read

		! Move to the next loop increment
		cycle headerloop
	    else
		!print*, "going back one space"

		! Rewind the file record one space
		backspace (unit_hdr_file)
		
		! Read line and look for the key words
		read(unit_hdr_file,*, iostat=ioerr) keyword1, keyword2, input_header_value
		if (ioerr .gt. 0) then
		    write(*,'(A,I0,A,A,A,A)') "ERROR ", ioerr, &
					    ": Problem reading the line containing: ", &
					    keyword1, keyword2, input_header_value
		    stop
		elseif (ioerr .eq. iostat_end) then
		    ! End of File...Exit the loop
		    exit headerloop
		endif
		
		if (keyword1 .eq. "set") then !!! PMB move this into iostat check?
		    if (keyword2 .eq. "NumberOfRows:") then
			NROW = input_header_value
		    elseif (keyword2 .eq. "NumberOfCol:") then
			NCOL = input_header_value
		    elseif (keyword2 .eq. "NumberOfLay:") then
			NLAY = input_header_value
		    elseif (keyword2 .eq. "NumberOfStressPeriods:") then
			NSP = input_header_value
		    elseif (keyword2 .eq. "NumberOfTimeSteps:") then
			NSTEP = input_header_value
		    elseif (keyword2 .eq. "NumberOfCellByCell:") then
			NCBC = input_header_value
		    elseif (keyword2 .eq. "NumberOfLakes:") then
			NUM_LAKES = input_header_value
		    elseif (keyword2 .eq. "NumberOfLakeCells:") then
			NUM_LAKE_CELLS = input_header_value
		    elseif (keyword2 .eq. "LakeHeadsLayer:") then
			LAKE_HDS_LAYER = input_header_value
		    endif
		endif
	    endif
	enddo headerloop
	
	close(unit_hdr_file)
	
	print*, "NROW,NCOL,NLAY,NSP,NSTEP,NCBC,NUM_LAKES,NUM_LAKE_CELLS,LAKE_HDS_LAYER"
	print*, NROW,NCOL,NLAY,NSP,NSTEP,NCBC,NUM_LAKES,NUM_LAKE_CELLS,LAKE_HDS_LAYER
	print*, ""
	
	! Allocate array memory
	allocate ( STRESS_PERIOD(NSP) )
	allocate ( IROW_LAKES(NUM_LAKE_CELLS), ICOL_LAKES(NUM_LAKE_CELLS) )
	allocate ( HDS_LAKE(NUM_LAKE_CELLS,NSP) )
	allocate ( AVERAGE_HDS_LAKE(NUM_LAKES,NSP) )
	allocate ( HDS(NCOL,NROW,NLAY) )
	! ---------------------------------------------------------
	!
	! END READ THE HEADER CONTROL FILE AND SET THE ARRAY SIZES
	!
	! ---------------------------------------------------------
	
	

	! =========================================================
	!
	!	READ THE STRESS PERIOD FILE
	!
	! =========================================================
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
	! ---------------------------------------------------------
	!
	!	END READ STRESS PERIOD
	!
	! ---------------------------------------------------------


	! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	!
	!	LAKE HEADS AND SPRINGFLOW SECTION
	!
	! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


	! =========================================================
	!
	!	READ THE SORTED LAKE CELL FILE
	!
	! =========================================================
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
	! ---------------------------------------------------------
	!
	!	END READ SORTED LAKE CELL FILE
	!
	! ---------------------------------------------------------


	! =========================================================
	!	OPEN AND READ SPRING DRN FILE
	!
	!	OPEN AND READ THE BINARY HEADS FILE
	!
	!	OPEN AND WRITE TO SPRINGFLOW OUTPUT FILE NAME
	! =========================================================

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
		if (ILAY .EQ. LAKE_HDS_LAYER) then
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
		if (PELEV .eq. -0.0) PELEV = 0.0
		

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
	
	! Deallocate array memory
	deallocate ( STRESS_PERIOD )
	deallocate ( IROW_LAKES, ICOL_LAKES )
	deallocate ( HDS_LAKE )
	deallocate ( AVERAGE_HDS_LAKE )
	deallocate ( HDS )

	! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    enddo inputloop

    close(unit_contfle)
    ! ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
    !
    !   		END READ THE MASTER CONTROL FILE
    !			     END MARCH THROUGH LIST
    !
    ! ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo


!############################################################################
!
END PROGRAM MAIN
!
!############################################################################
