/**
 * A quick code to extract the first 5 columns of the
 * drn file. Each Stress Period is extracted and output to an
 * independent output file.
 * 
 * The purpose of this extraction is to allow the user to
 * determine unique rows using whatever tool is preferred
 * by the user.
 * 
 * This code was originally inspired by the problem of merging
 * Spring Key information from one drn file to another drn file.
 * In order to associate the correct Spring Key to the correct
 * row, a certain set of columns had to be used as a unique
 * identifier of the Spring within the drn file in each of
 * the Stress Periods.
 * 
 * Compile with:
 * g++ -g extract_springs_eachSP_put_each_to_file.cc -o extract_springs_eachSP_put_each_to_file.exe
 * 
 * 
 * Written 20191010. PMBremner
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
	if (argc != 4)
	{
		std::cout << "\nInvalid number of arguments was provided!\n\n";
		return EXIT_FAILURE;
	}
	
	// Read in the filenames from stdin
	std::string drn_file = argv[1];
	std::string outfilename_prefix = argv[2];
	std::string numSP_str = argv[3];
	std::istringstream input(numSP_str);
	unsigned int numSP_tmp;
	input >> numSP_tmp;
	
	// Set up the opening and reading of input files ...
	std::ifstream in_drn;
	in_drn.open(drn_file);
	
	// ...of the output file
	std::ofstream out_drn;
	
	std::string tmp1, tmp2;
	
	
	const unsigned int numSP = numSP_tmp;
	std::vector<unsigned int> lyrs, rows, cols;
	std:vector<double> poolelv, conduc;
	std::vector<std::string> keys;
	
	
	
	// Get the header information
	// From in_drn to out_drn
	std::getline(in_drn,tmp1);
	std::getline(in_drn,tmp1);
	std::getline(in_drn,tmp1);
	
	
	// Get the number of rows between stress periods
	unsigned int numrows=0;
	unsigned int numrows_new=0;
	std::getline(in_drn,tmp1);
	std::istringstream line_in(tmp1);
	line_in >> numrows_new;
	
	
	// Cycle through all the Stress Periods.
	// Determine the number of rows within each
	// Stress Period as we go along.
	for (unsigned int n=0; n<numSP; ++n)
	{
	  std::ostringstream nstrm;
	  nstrm << n;
	  std::string outfilename = (outfilename_prefix + "_" + nstrm.str() + ".dat" );
	  out_drn.open(outfilename);
	  
	  // Read through the rows within current SP
	  // in_drn
	  if (n==0) numrows = numrows_new;
	  for (unsigned int i=0; i<(numrows+1); ++i)
	  {
	    
	    unsigned int lyr_in, rows_in, cols_in, key_in;
	    double poolelv_in, conduc_in;
	    
	    std::getline(in_drn,tmp1);
	    std::istringstream line_in(tmp1);
	    
	    if (i==numrows)
	    {
	      if (n!=numSP-1)
	      {
		//out_drn << tmp1 << "\n";
		line_in >> numrows_new;
	      }
	    }
	    else
	    {
	      //out_drn << tmp1 << "\n";
	      line_in >> lyr_in >> rows_in >> cols_in;
	      line_in >> poolelv_in;
	      line_in >> conduc_in;
	      line_in >> key_in;
	      
	      out_drn << lyr_in;
	      out_drn << " " << rows_in;
	      out_drn << " " << cols_in;
	      out_drn << " " << poolelv_in;
	      out_drn << " " << conduc_in;
	      //out_drn << " " << key_in;
	      out_drn << "\n";
	    }
	  }
	  std::cout << n << " " << numrows_new << "\n";
	  
	  // Reset the number of Rows to the current set value
	  numrows = numrows_new;
	  
	  // Close current output file
	  out_drn.close();
	}
	
	// Close input file
	in_drn.close();
}




