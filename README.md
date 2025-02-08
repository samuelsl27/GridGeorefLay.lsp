# GridGeorefLay.lsp
Description
This AutoLISP script creates a georeferenced grid in BricsCAD or AutoCAD by drawing coordinate markers and labels along the borders of a selected viewport. It automatically formats numbers (using spaces as thousand separators) and can either use default settings or prompt the user for manual inputs to control the appearance and positioning of the grid elements.

Key Features
Default vs. Manual Settings:
Choose to use preconfigured default settings or enter parameters manually.

Automatic Layer Creation:
The script ensures that two layers, GridGeorefLay_marks (for the lines) and GridGeorefLay_text (for the coordinate labels), exist, creating them if necessary.

Dynamic Number Formatting:
Numbers are formatted with spaces as thousands separators for better readability.

Customizable Text and Line Placement:
Decide whether text labels are drawn inside or outside the viewport and whether grid lines are drawn inside, on, or outside the border.

Step Size Calculation:
When using defaults (or by choosing the predefined option in manual mode), the script calculates an appropriate step size so that the grid marks appear at “round” numbers (e.g., 0, 50,000, 100,000, etc.) based on an allowed list of measures.

Configuration Options and Prompts
When running the command GRIDGEOREFLAY in AutoCAD, the script will guide you through several prompts:

Default Settings Prompt:

Prompt: Do you want to use the default settings? [Y/N]:
If "Y":
Text Position: Forced to Outside (indicated by “O”).
Line Placement: Forced to On the border (indicated by “ON”) which centers the lines on the edge.
Starting Coordinates: Both X and Y start at 0.
Text Offset: The distance from the frame is set to 3.
Line Length: Uses the Short (S) option.
Step Size: Calculated automatically so that, starting from 0, marks are placed at round numbers while ensuring the separation is three times the estimated text width.
If "N":
The script will request manual input for several parameters.
Text Placement Option (if not using defaults):

Prompt: Do you want to draw the text inside or outside? [I/O]:
Options:
I: Draw text Inside the viewport frame.
O: Draw text Outside the viewport frame.
Line Placement Option (if not using defaults):

Prompt: Do you want to draw the lines inside, on or outside? [I/ON/O]:
Options:
I: Draw lines Inside the frame.
ON: Draw lines On the frame (centered on the border).
O: Draw lines Outside the frame.
Manual Parameter Inputs (if not using defaults):

Starting Coordinates:
Prompt: Specify starting value for x coordinates:
Sets the initial x-coordinate value for the grid labels.
Prompt: Specify starting value for y coordinates:
Sets the initial y-coordinate value for the grid labels.
Text Distance:
Prompt: Specify text distance from frame:
Determines the offset distance between the grid frame and the coordinate text.
Line Length Option:
Prompt: Select line length option [L/M/S]:
Options:
L: Long line length (base length equals the text offset plus five times the current text size).
M: Medium line length (half of the base length).
S: Short line length (quarter of the base length).
Step Size:
Prompt: Specify step size for coordinates (or enter 'p' for predefined calculation):
Options:
Enter a specific numeric step size.
Enter p (or P) to compute the step size automatically (using a candidate measure chosen from an allowed list such as 25, 50, 100, 200, 500, 1000, …, up to 1,000,000).
How It Works
Viewport Selection:
The script starts by prompting the user to select one or more viewports. It then extracts the model and paper space dimensions of the selected viewport.

Calculating the Frame:
The function #VPT_BOX calculates the four corner points (in model space) of the viewport. These points define the drawing area where the grid will be applied.

Defining Grid Parameters:
Based on the user’s choice (default or manual), the script sets the following:

The fixed axis for placing labels.
The direction for drawing grid lines and text (which varies depending on whether elements are drawn inside, on, or outside the frame).
The step size for coordinate markers.
The base and final line lengths based on the selected line length option.
Drawing the Grid:
The function draw-edge-labels is called four times to draw the grid markers and labels along each edge (bottom, top, left, right) of the viewport. For each marker:

A line is drawn in the GridGeorefLay_marks layer.
A text label (formatted with thousand separators) is placed in the GridGeorefLay_text layer.
Restoring System Variables:
After drawing the grid, the script restores any system variables changed during execution to maintain the AutoCAD environment’s integrity.

Installation and Usage
Requirements
AutoCAD with AutoLISP support.
Installation
Download the GridGeorefLay_v2_4.lsp file from this repository.
Open AutoCAD and load the file using the APLOAD command.
Run the command GRIDGEOREFLAY to launch the script.
Adding Videos and Example Images
To help users better understand the workflow, you can add explanatory videos and example images. Upload your media files to a hosting service (e.g., YouTube, Dropbox, Google Drive) and include them in this document as follows:

Embedding Videos
markdown
Copiar
Editar
[![Explanatory Video 1](https://img.youtube.com/vi/VIDEO_ID1/0.jpg)](https://www.youtube.com/watch?v=VIDEO_ID1)

[![Explanatory Video 2](https://img.youtube.com/vi/VIDEO_ID2/0.jpg)](https://www.youtube.com/watch?v=VIDEO_ID2)
Embedding Example Images
markdown
Copiar
Editar
![Example 1](path/to/image1.png)
![Example 2](path/to/image2.png)
Simply replace VIDEO_ID1, VIDEO_ID2, and the image paths with your actual media identifiers and URLs.

Contact
For additional information or if you have any questions, please feel free to connect on LinkedIn.

