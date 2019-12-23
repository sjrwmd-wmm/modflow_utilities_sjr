/**
 * This code is meant to append trailing identifier key
 * information to a MODFLOW drain file (the file to be appended).
 * The key information is read from a reference drain
 * file, and transferred to the need-to-be-appended file.
 * This code assumes that the number of lines and their order
 * are identical between the need-to-be-appended and
 * reference files. Please verify that this is the case
 * before running this program!
 * 
 * Usage:
 * The following lines are columns of user input arguments.
 * These arguments should all be input on the same line.
 * Names within <> should be replaced with the correct names.
 * 
 * <PATH>/correct_drn_file_from_ref.exe
 * <ref_drain_filename>
 * <need-to-be-appended_drain_filename>
 * <output_filename>
 * <conductance_multiplication_factor>
 * <number_of_stress_periods>
 * 
 * 
 * This code can be modified (with relatively little effort) to
 * match-up lines from one file to another, rather than simply assume
 * the number of lines are the same and in the same order. This change
 * is easy to make in the code, but the number and content of columns
 * used to match between files MUST constitute a unique identifier
 * of each row. For example, it was shown for the ECFTX model
 * drain files that the combination of the first four columns
 * (layer, row, column, pool-elevation) do NOT always uniquely identify
 * a row in the file. In this scenario a different method was used to
 * verify the line order.
 * 
 * 
 * Compile with:
 * g++ -g correct_drn_file_from_ref.cc -o correct_drn_file_from_ref.exe
 * 
 * 
 * Written 20191008. PMBremner
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


int main (int argc, char* argv[])
{
	
	// Check that the number of arguments are correct
	if (argc != 6)
	{
		std::cout << "\nFive arguments are required to run this code!\n\n";
		
		std::cout << "This code is meant to append trailing identifier key\n";
		std::cout << "information to a MODFLOW drain file (the file to be appended).\n";
		std::cout << "The key information is read from a reference drain\n";
		std::cout << "file, and transferred to the need-to-be-appended file.\n";
		std::cout << "This code assumes that the number of lines and their order\n";
		std::cout << "are identical between the need-to-be-appended and\n";
		std::cout << "reference files. Please verify that this is the case\n";
		std::cout << "before running this program!\n\n";
		
		std::cout << "Usage:\n";
		std::cout << "The following lines are columns of user input arguments.\n";
		std::cout << "These arguments should all be input on the same line.\n";
		std::cout << "Names within <> should be replaced with the correct names.\n\n";
		
		std::cout << "<PATH>/correct_drn_file_from_ref.exe\n";
		std::cout << "<ref_drain_filename>\n";
		std::cout << "<need-to-be-appended_drain_filename>\n";
		std::cout << "<output_filename>\n";
		std::cout << "<conductance_multiplication_factor>\n";
		std::cout << "<number_of_stress_periods>\n\n";
		
		std::cout << "Example:\n";
		std::cout << "../../bin/correct_drn_file_from_ref.exe ";
		std::cout << "../../model/ecftx_tr.20190730.drn ";
		std::cout << "ecftx_tr.20190730.drn.x0.01 ";
		std::cout << "ecftx_tr.20190730.drn.x0.01_complete ";
		std::cout << "0.01 ";
		std::cout << "133\n\n";
		
		return EXIT_FAILURE;
	}
	
	
	// save the current settings
	//ios::fmtflags old_settings = cout.flags();
	// save previous format flags
	//int old_precision = cout.precision();
	// save previous precision setf
	
	// Read input from stdin
	// -------------------------------------
	std::string ref_drn_file = argv[1];
	std::string drn_to_correct = argv[2];
	std::string outfilename = argv[3];
	
	std::string multfactor_str = argv[4];
	std::istringstream input1(multfactor_str);
	double multfactor;
	input1 >> multfactor;
	
	std::string numSP_str = argv[5];
	std::istringstream input2(numSP_str);
	unsigned int numSP_tmp;
	input2 >> numSP_tmp;
	// -------------------------------------
	
	
	
	// Set up the opening and reading of input files ...
	std::ifstream in_ref, in_drn;
	in_ref.open(ref_drn_file);
	in_drn.open(drn_to_correct);
	
	// ...and opening and writing of the output file
	std::ofstream out_drn;
	out_drn.open(outfilename);
	
	
	std::string tmp1, tmp2;
	
	
	const unsigned int numSP = numSP_tmp;
	std::vector<unsigned int> lyrs, rows, cols;
	std:vector<double> poolelv, conduc;
	std::vector<std::string> keys;
	
	
	
	// Get the header information
	// -------------------------------------
	
	// From in_drn to out_drn
	std::getline(in_drn,tmp1);
	out_drn << tmp1 << "\n";
	std::getline(in_drn,tmp1);
	out_drn << tmp1 << "\n";
	std::getline(in_drn,tmp1);
	out_drn << tmp1 << "\n";
	
	// From in_ref
	std::getline(in_ref,tmp1);
	std::getline(in_ref,tmp1);
	std::getline(in_ref,tmp1);
	// -------------------------------------
	
	
	// Get the number of rows for the first stress period
	unsigned int numrows=0, numrows_ref=0;
	unsigned int numrows_new=0, numrows_ref_new=0;
	std::getline(in_drn,tmp1);
	out_drn << tmp1 << "\n";
	std::istringstream line_in(tmp1);
	line_in >> numrows_new;
	
	std::getline(in_ref,tmp1);
	std::istringstream line_ref(tmp1);
	line_ref >> numrows_ref_new;
	
	
	
	// Cycle through all the Stress Periods.
	// Determine the number of rows within each
	// Stress Period as we go along.
	for (unsigned int n=0; n<numSP; ++n)
	{
	  // Read through the rows within current SP
	  // in_ref
	  if (n==0) numrows_ref = numrows_ref_new;
	  lyrs.resize(numrows_ref);
	  rows.resize(numrows_ref);
	  cols.resize(numrows_ref);
	  poolelv.resize(numrows_ref);
	  conduc.resize(numrows_ref);
	  keys.resize(numrows_ref);
	  
	  for (unsigned int i=0; i<(numrows_ref+1); ++i)
	  {
	    std::getline(in_ref,tmp1);
	    std::istringstream line_ref(tmp1);
	    //std::cout << i << "\n";
	    if (i==numrows_ref)
	    {
	      if (n!=numSP-1)
	      {
		//out_drn << tmp1 << "\n";
		line_ref >> numrows_ref_new;
	      }
	    }
	    else
	    {
	      // Re-initialize  previous value from arrays
	      lyrs[i] = 0;
	      rows[i] = 0;
	      cols[i] = 0;
	      
	      
	      line_ref >> lyrs[i] >> rows[i] >> cols[i] >> poolelv[i] >> conduc[i];
	      
	      
	      // Extract the remainder columns of Spring Key Descriptions
	      // at the end of each line. The number of key items is
	      // not consistent from line to line, so continue trying to
	      // extract columns until no longer able to.
	      // (line_ref >> item) returns true[false] when column found[not found].
	      // All the items found are appended into a single string.
	      std::string keyitems, item;
	      
	      while (line_ref >> item)
	      {
		keyitems += " ";
		keyitems += item;
	      }
	      
	      keys[i] = keyitems;
	    }
	  }
	  //std::cout << n << " " << numrows_ref_new << "\n";
	  
	  // Reset the number of Rows to the current set value
	  numrows_ref = numrows_ref_new;
	  
	  
	  
	  // Read through the rows within current SP
	  // in_drn
	  if (n==0) numrows = numrows_new;
	  for (unsigned int i=0; i<(numrows+1); ++i)
	  {
	    
	    unsigned int lyr_in, rows_in, cols_in;
	    double poolelv_in, conduc_in;
	    
	    std::getline(in_drn,tmp1);
	    std::istringstream line_in(tmp1);
	    
	    if (i==numrows)
	    {
	      
	      if (n!=numSP-1)
	      {
		out_drn << tmp1 << "\n";
		line_in >> numrows_new;
	      }
	    }
	    else
	    {
	      line_in >> lyr_in >> rows_in >> cols_in >> poolelv_in >> conduc_in;
	      
	      // Portion commented out matches lines from the in_ref
	      // without the assumption of the same line order
// 	      for (unsigned int n=0; n<lyrs.size(); ++n)
// 	      {
// 		if (lyr_in==lyrs[n])
// 		{
// 		  if (rows_in==rows[n])
// 		  {
// 		    if (cols_in==cols[n])
// 		    {
// 		      if (poolelv_in==poolelv[n])
// 		      {
// 			//if (conduc_in==conduc[n]*multfactor)
// 			//{
// 			  out_drn << lyr_in << " " << rows_in << " ";
// 			  out_drn << cols_in << " " << poolelv_in;
// 			  out_drn << " " << conduc[n]; //conduc_in;
// 			  out_drn << keys[n] << "\n";
// 			  break; // exit the loop
// 			//}
// 		      }
// 		    }
// 		  }
// 		}
// 	      }
	      
	      //out_drn << tmp1 << "\n";
	      
	      out_drn << lyr_in;
	      out_drn << " " << rows_in;
	      out_drn << " " << cols_in;
	      //out_drn.setf(ios::fixed, ios::floatfield);  // set fixed floating f
	      out_drn << fixed << setprecision(2) << setw(8);
	      out_drn << " " << poolelv_in;
	      out_drn << fixed << setprecision(1) << setw(12);
	      out_drn << " " << conduc_in;
	      out_drn << setprecision(0);
	      out_drn << keys[i]; // already has space at the beginning
	      out_drn << "\n";
	    }
	  }
	  std::cout << "SP: " << n+1 << "  Num Rows: " << numrows_new << "\n";
	  
	  // Reset the number of Rows to the current set value
	  numrows = numrows_new;
	}
	
	
	in_ref.close();
	
	// Close all files
	in_drn.close();
	out_drn.close();
}




