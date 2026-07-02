# Summary (R_examination_stat)

- This program generates score reports when conducting mock exams for the Japanese Registered Dietitian National Examination.
- The report card will be created as a PDF containing the overall exam score, individual score sheets (Report Card 1), and a report card (Report Card 2) detailing the percentage of correct answers after each question.
- In addition, statistical data on exam scores, individual student scores by subject area, and data on correct and incorrect answers for each question will be output in CSV format.

# Input data

- The Registered Dietitian National Examination is a 200-question multiple-choice exam where you select one answer from five options.
- The following data is needed to create the report card:
  - All of these should be provided in CSV file format.
  - The Japanese code is formatted as Shift-JIS (cp932) for execution on Japanese version of Windows.
  - Please create the file in Excel and save it as a CSV file.
- Correct Answer Data: As correct answer data, prepare 200 correct answers in a CSV file in the following format.

| Question No.   | 1   | 2   | ... | 200 |
|----------------|-----|-----|-----|-----|
| Correct Answer | 3   | 4   | ... | 5   |

- Answer data of each student: Since mock exams often use mark sheet readers, we prepare a CSV file containing the following items as general output data.

| Student ID | Name  | Question1 | Question2 | ... | Question200 |
|------------|-------|-----------|-----------|-----|-------------|
| 1001       | name1 | 3         | 4         | ... | 5           |
| 1002       | name2 | 2         | 4         | ... | 5           |

- The data is intended to be created on a Japanese Windows system, but the R script itself will also work on Linux. (Confirmed on Ubuntu 22.04)

# Setup (Preparing for execution)

- Files with ".R" are R scripts. Please configure the following at the beginning: \# --- Settings --- including test_date, etc.
- If the library is missing, please install it.

# Execution

- Place your data in the `data` folder, open the R script with the `.R` extension in RStudio or a similar program, and run it using `source`.
- To add a stamp, you will need the files in the `img` folder.

# Output result

- The `test_date` command creates a subfolder where test results and other data are output.
- Individual report cards are created as PDFs for each student within the `individual_report` folder.
