# RNN and Auto_Regression - Automation of the Data Preparation

When using the RNN or Auto Regression method, there is some pre-work quite lengthy to be done in preparing the data to be used in as an array format for the RNN or as a data frame for the Auto Regression. The idea of this code to take the work off your shoulder and get the final data ready for either the RNN implementation or the Auto Regression. This code has been implemented in the file named RNN_Preparation_Function.r with the function named AR_prep.

The input of this function is:

    the data frame on hand
    the name of the x columns to be considered from this data frame
    the name of the y column to be considered
    the lag variable: how much time is the lagging time
    RNN whether True or False: if True is chosen, then the output is related to the RNN method and if False is chosen, then the output is related to the Auto Regression method.

The output of this function is a list of two variable:

    the first variable is the newly implemented data frame to be used in the RNN or Auto Regression
    the second variable is the new response vector to be used

Should you have any query or suggestion, please drop me a line. I will be more than happy to assist you.

Best regards,
Mustafa Anjrini
