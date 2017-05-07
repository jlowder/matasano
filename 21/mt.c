// This is a C version, made from the pseudocode from the wikipage - it was
// almost C language already. 


#include <stdio.h>
 // Create a length 624 array to store the state of the generator
unsigned int MT[624];
int idx = 0;
 
// Initialize the generator from a seed
void initialize_generator(int seed)
{
    int i = 0;
    MT[0] = seed;
    for (i=1; i<=623; i++) // loop over each other element
    { 
        MT[i] = 0xFFFFFFFF & (1812433253 * (MT[i-1] ^ (MT[i-1]>>30)) + i); 
    }
    
    idx = 0;
 }

// Generate an array of 624 untempered numbers
void generate_numbers()
 {
     int i=0;
     for (i=0;i<624;i++)
     {
         unsigned int y = (MT[i] & 0x80000000) + (MT[(i+1) % 624] & 0x7fffffff);   // bits 0-30 (first 31 bits) of MT[...]
         MT[i] = MT[(i + 397) % 624] ^ (y>>1);
         if ((y % 2) != 0)
         { // y is odd
             MT[i] = MT[i] ^ (2567483615); // 0x9908b0df
         }
     }
 }
 
 // Extract a tempered pseudorandom number based on the idx-th value,
 // calling generate_numbers() every 624 numbers
unsigned  int extract_number()
 {
     if (idx == 0)
     {
         generate_numbers();
     }
 
     unsigned int y = MT[idx];
     y = y ^ (y>>11);
     y = y ^ ((y<<7) & 2636928640);
     y = y ^ ((y<<15) & (4022730752));
     y = y ^ (y>>18);

     idx = (idx + 1) % 624;
     return y;
 }
 
main()
{
    int i=0;

    initialize_generator(1);

    for (i=0;i<5;i++)
    {
        printf("%u\n", extract_number());
    }

}
