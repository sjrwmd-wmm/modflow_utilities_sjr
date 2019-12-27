/**
 * Code to alter the values of a drn file through:
 *      1) addition
 *      2) multiplication
 * 
 * This code reads in *all* the existing columns of
 * a drn file and outputs all columns to a new drn
 * file with the altered values.
 * 
 * Command-line Argument Inputs:
 *      1) input drn filename
 *      2) output filename
 *      3) modification mathematical operator (addition or multiplication)
 *      4) modification value (what to add or multiply to each current spring conductance value)
 *      5) output precision (how many decimal places to write out)
 * 
 * 
 * Compile with:
 * g++ -g modify_drn_file_all_values.cc -o modify_drn_file_all_values.exe
 * 
 * 
 * 
 * ---------------------------------------------------
 * How to convert DOS file to UNIX file in vim. Proceedure from:
 * https://unix.stackexchange.com/questions/32001/what-is-m-and-how-do-i-get-rid-of-it
 * 
 * :e ++ff=dos
 * 
 * The :e ++ff=dos command tells Vim to read the file again, forcing dos file format. Vim will remove CRLF and LF-only line endings, leaving only the text of each line in the buffer.
 * 
 * then
 * 
 * :set ff=unix
 * 
 * and finally
 * 
 * :wq
 * ---------------------------------------------------
 * 
 * 
 * 
 * Written 20191223. PMBremner
 */

#include <iostream>
#include <sstream>
#include <iomanip>
#include <fstream>

#include <vector>
#include <cmath>
#include <iterator>

#include <string>
#include <map>
#include <set>
#include <random>
#include <time.h>
#include <algorithm>


using namespace std;


// --- PROGRAM INFORMATION ---

// The following struct provides the complete documentation
// for this program. The documentation is divided among
// themes that are displayed n the terminal when the User
// supplies the "--help" or "-h" as an argument to the code.
// If no arguments are provided, then only the USAGE
// statement appears.
//
// Documentation updates should be done in this struct
// so that it can appear to the User when requested.

struct ProgramInfo
{
    
    std::string purpose = (
"The purpose of this code is to modify the hydrologic\n"
"spring conductance values contanined in the drn package\n"
"file used in MODFLOW. This code provides the method to\n"
"change ALL the conductance values by a user specified amount.\n"
"This code (currently) does not provide a way to modify\n"
"only a specific subset of values. The program will read-in\n"
"a user specified input drn file, and create a new drn file\n"
"containing the altered values. All other information on\n"
"each file line (including any extra trailing information)\n"
"is preserved from the original file, and output to the new\n"
"drn file.\n"
"\n\n"
"Each conductance value will be altered with\n"
"the user specified mathematical operation.\n"
"The current alteration options are:\n"
"\t- addition\n"
"\t- multiplication\n"
"\n"
"The program may be executed in any directory so long as the input and\n"
"output filenames (as provided in the command-line arguments) include\n"
"the absolute or relative PATHs.\n");

    
    std::string inputdescrip = (
"INPUT FILE DESCRIPTION:\n"
"    An input drn file which needs to be altered must be provided.\n"
"    It is a space-delimited file that contains model information for\n"
"    a list of one or more springs, and repeats the list for one or more\n"
"    Stress Periods. Each Stress Period is preceeded by a header line\n"
"    that describes how many springs are within that period.\n"
"    Within a Stress Period, the program expects each file line to\n"
"    correspond to one spring.\n\n"
"    Column Descriptions of Each File Line of Spring Information:\n"
"    \t1) Layer in model grid to which the spring is connected [integer]\n\n"
"    \t2) Row in the model grid that the spring is located [integer]\n\n"
"    \t3) Column in the model grid that the spring is located [integer]\n\n"
"    \t4) Pool elevation of the spring [double float]\n\n"
"    \t5) Hydrologic spring conductance [double float]\n\n"
"    \t6) Any other optional information [variable format, read in as string]\n"
"\n");


    std::string usage = (
"USAGE:\n"
"The following lines are columns of user input arguments.\n"
"These arguments should all be input on the same line.\n"
"Names within <> should be replaced with the correct names.\n"
"\n"
"    <PATH>/modify_drn_file_all_values.exe\n"
"    <input_drn_filename>\n"
"    <output_filename>\n"
"    <modification_mathematical_operator_(addition_or_multiplication)>\n"
"    <modification_value_(what_to_add_or_multiply_to_each_current_spring_conductance_value)>\n"
"    <output_precision_(how_many_decimal_places_to_write_out)>\n"
"\n"
"For complete documentation supply one of the optional arguments '--help' or '-h'\n");


    std::string compile = (
"COMPILING:\n"
"Originally compiled with GNU compilers.\n"
"Compile with (just 1st line, names in <> defined below):\n"
"     g++ -g <cc_filename> -o <exe_filename>\n"
"for\n"
"     cc_filename = modify_drn_file_all_values.cc\n"
"     exe_filename = bin/modify_drn_file_all_values.exe\n");


    std::string dependencies = (
"CODE DEPENDENCIES For Compilation:\n"
"     No non-standard dependencies.\n");


    std::string authorship = "Written 20191223. PMBremner\n";
};




int main (int argc, char* argv[])
{
    
    
    // Bring in the Program Information
    ProgramInfo pinfo;
    
    
    for (unsigned int i=0; i<argc; ++i)
    {
        std::string argument = argv[i];
        
        if ( argument == "--help" || argument == "-h")
        {
            std::cout << "\n" << pinfo.purpose << "\n\n";
            std::cout << pinfo.inputdescrip << "\n\n";
            std::cout << pinfo.usage << "\n\n";
            std::cout << pinfo.compile << "\n\n";
            std::cout << pinfo.dependencies << "\n\n";
            std::cout << pinfo.authorship << "\n";
            return EXIT_SUCCESS;
        }
    }
    
    
    // Check that the number of arguments are correct
    if (argc != 6)
    {
        std::cout << "\nERROR: Invalid number of command-line arguments were provided!\n\n";
        
        std::cout << "\n" << pinfo.usage << "\n\n";
        
        return EXIT_FAILURE;
    }

    // Read in Command-line Argument Inputs from stdin
    // -----------------------------------------------
    //      ARG 1) input drn filename
    std::string drn_file = argv[1];
    
    //      ARG 2) output filename
    std::string outfilename = argv[2];
    
    //      ARG 3) modification mathematical operator (addition or multiplication)
    std::string mathoperator = argv[3];
    
    //      ARG 4) modification value (what to add or multiply to each current spring conductance value)
    std::string modvalue_str = argv[4];
    std::istringstream argv4_in(modvalue_str);
    double modvalue;
    argv4_in >> modvalue;
    
    //      ARG 5) output precision (how many decimal places to write out)
    std::string outprec_str = argv[5];
    std::istringstream argv5_in(outprec_str);
    double outprec;
    argv5_in >> outprec;
    // -----------------------------------------------
    
    
    
    // Check that the mathematical operation is on the list
    if (mathoperator != "addition" && mathoperator != "multiplication")
    {
        // TERMINATE THE PROGRAM !!!
        std::cout << "\nERROR:    Invalid mathematical operator input!\n\n";
        std::cout << "Options are: 'addition' or 'multiplication'\n";
        std::cout << "Check command-line inputs and run again\n\n";
        
        return EXIT_FAILURE;
    }

    
    // Set up the opening and reading of input files ...
    std::ifstream in_drn;
    in_drn.open(drn_file);

    // ...of the output file
    std::ofstream out_drn;
    out_drn.open(outfilename);

    std::string tmp1, tmp2;
    
    
    
    // Get past all the header lines that might exist
    // Start paying attention at the line that starts with "PARAMETER"
    // Dump each line directly into the output file
    bool headerflag = true;
    while (headerflag)
    {
        std::string headervalue;
        
        // Read in the next line
        std::getline(in_drn,tmp1);
        std::istringstream line_in(tmp1);
        line_in >> headervalue;
        
        // Dump the line into the output file
        out_drn << tmp1 << "\n";
        
        // Check if we've finished with the header lines yet
        // Get out of the while loop when complete
        if (headervalue == "PARAMETER") headerflag = false;
    }
    
    
    // Read in and dump one more line before Stress Period 1 begins
    std::getline(in_drn,tmp1);
    out_drn << tmp1 << "\n";
    
    
    
    // Cycle through all the Stress Periods.
    // Determine the number of rows within each
    // Stress Period as we go along.
    while (std::getline(in_drn,tmp1))
    {
        
        unsigned int numrows=0;
        
        // Output the whole line to file
        out_drn << tmp1 << "\n";
        
        // Now capture the number of rows within the current Stress Period
        std::istringstream line_in2(tmp1);
        line_in2 >> numrows;
        
        
        // Read through the rows within current Stress Period
        for (unsigned int i=0; i<numrows; ++i)
        {
            
            unsigned int lyr_in, rows_in, cols_in;
            double poolelv_in, conduc_in;
            
            std::string lyr_in_str, rows_in_str, cols_in_str;
            std::string poolelv_in_str, conduc_in_str;

            std::getline(in_drn,tmp1);
            std::istringstream line_in3(tmp1);

            
            line_in3 >> lyr_in >> rows_in >> cols_in >> poolelv_in_str >> conduc_in_str;
            
            
            // Convert pool_elevation and spring_conductance to doubles
            // TODO:    poolelv_in_str is converted to double poolelv_in, but
            //          poolelv_in isn't used at this time.
            //          This exists to allow for the future option to modify
            //          this value, as well
            std::istringstream poolelv_st(poolelv_in_str);
            poolelv_st >> poolelv_in;
            
            std::istringstream conduc_st(conduc_in_str);
            conduc_st >> conduc_in;
            
            
            // Extract the remainder of columns of Spring Key Descriptions
            // at the end of each line. The number of key items is not
            // consistent from line to line, so continue trying to
            // extract columns until no longer able to.
            //
            // (line_ref >> item) returns true[false] when column found[not found].
            // All the items found are appended into a single string.
            std::string keyitems, item;

            while (line_in3 >> item)
            {
                keyitems += " ";
                keyitems += item;
            }
            
            
            // Alter the Spring Conductance value
            // TODO:    There might be a smarter way of doing
            //          this so that an if statement isn't
            //          required every loop iteration
            if (mathoperator == "addition")
            {
                conduc_in = conduc_in + modvalue;
            }
            else if (mathoperator == "multiplication")
            {
                conduc_in = conduc_in * modvalue;
            }
            
            
            // Output to file
            out_drn << lyr_in;
            out_drn << " " << rows_in;
            out_drn << " " << cols_in;
            out_drn << " " << poolelv_in_str; // poolelv_in // TODO: switch if pool_elevation is altered
            
            //out_drn << fixed << setprecision(1) << setw(12);
            // NOTE:    Use this syntax to ensure that the precision sets
            //          the number of decimal places, and not change the
            //          numerical format (i.e., float to scientific notation)
            out_drn << fixed << setprecision(outprec);
            
            out_drn << " " << conduc_in;
            
            // Reset the precision to the default
            out_drn << setprecision(0);
            
            out_drn << keyitems;
            out_drn << "\n";
            
            
//             out_drn << lyr_in;
//             out_drn << " " << rows_in;
//             out_drn << " " << cols_in;
            //out_drn << fixed << setprecision(2) << setw(8);
//             out_drn << " " << poolelv_in;
            //out_drn << fixed << setprecision(1) << setw(12);
//             out_drn << " " << conduc_in;
            //out_drn << setprecision(0);
//             out_drn << keyitems;
//             out_drn << "\n";
        }
    }

    // Close files
    in_drn.close();
    out_drn.close();
}




