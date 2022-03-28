#include <stdio.h>
#include <stdlib.h>
#include "../device_properties.h"

struct device_properties *dp1 = NULL;
struct device_properties *dp2 = NULL;

int success_count = 0;
int failure_count = 0;
int tests_count = 0;

void compare(char *msg, bool expected_result)
{
  tests_count++;
  printf("=============================\n");
  printf("%s\n", msg);
  bool match = compare_device_properties(dp1, dp2);
  match == expected_result ? success_count++ : failure_count++;
  printf("MATCH: %s\n", match ? "TRUE" : "FALSE");
  printf("=============================\n");
  printf("dp1's properties:\n");
  print_device_properties(dp1);
  printf("\n");
  printf("dp2's properties:\n");
  print_device_properties(dp2);
  printf("-----------------------------\n");
  printf("-----------------------------\n\n");
}

int main()
{
  printf("Comparing 2 `device_properties`.\n\n");
  printf("Note: first one has higher priority.\n");
  printf("e.g. compare(user provided identifiers, keyboard's properties)\n\n");

  compare("1. Compare: when both are NULL", true);

  dp1 = calloc(1, sizeof *dp1);
  dp1->product_name = "A Keyboard";
  compare("2. Compare: when 2nd is NULL", false);

  dp2 = calloc(1, sizeof *dp2);
  dp2->manufacturer = "Manufacturer";
  compare("3. Compare: different string properties", false);

  dp2->product_name = "A Keyboard";
  compare("4. Compare: same string property | dp2 - another string property", false);

  dp1->location_id = 1234;
  compare("5. Compare: same string property | dp2 - another string property | dp1 - number property", false);

  dp2->vendor_id = 1234;
  compare("6. Compare: same string property | dp2 - another string property | different number property", false);

  dp1->vendor_id = 1234;
  dp2->location_id = 1234;
  compare("7. Compare: same properties - string(1), number(2) | dp2 - another string property", false);

  dp1->manufacturer = "Manufacturer";
  compare("8. Compare: same properties - string(2) , number(2)", true);

  dp1->serial_number = "Serial Number";
  dp2->serial_number = "Serial Number";
  dp1->transport = "USB";
  dp2->transport = "USB";
  dp1->product_id = 14;
  dp2->product_id = 14;
  dp1->vendor_id = 73;
  dp2->vendor_id = 73;
  dp1->country_code = 66;
  dp2->country_code = 66;
  dp1->version_number = 89;
  dp2->version_number = 89;
  compare("9. Compare: all properties are SAME", true);

  printf("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%");
  printf("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n");
  printf("TOTAL: %d  | SUCCESS: %d  | FAILURE: %d\n",
          tests_count, success_count, failure_count);
  printf("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%");
  printf("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n");
  return 0;
}
