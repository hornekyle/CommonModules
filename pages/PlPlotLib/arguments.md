title: PlPlotLib Common Arguments

Common Arguments
----------------

A number of arguments are accepted by many routines in the PlPlotLib 
module due to their common applicability. To prevent duplication of 
effort, these arguments are documented here with the expectation that 
they behave in a consistent/expected manner for each of the routines 
that accept them.

Deviations from these standard behaviors or routine-specific extensions 
(if any) can be found in the documentation for each routine.

### `color`

The color of various plot components may be set using a `character` value,
for example `color='red'`. Acceptable values include the following:

	* 'k', 'black'   :: Black
	* 'w', 'white'   :: White
	* 'r', 'red'     :: Red
	* 'g', 'green'   :: Green
	* 'b', 'blue'    :: Blue
	* 'm', 'magenta' :: Magenta
	* 'y', 'yellow'  :: Yellow
	* 'c', 'cyan'    :: Cyan
	* 'fg'           :: Foreground
	* 'bg'           :: Background

Additionally, the `character` value may contain an ascii decimal encoding of
`real` number between zero and one.
In this case, the color will be taken from the continuous colormap instead
of the discrete indexed colors. For example: `color='  0.534 '` This can
easily be automated through the use of internal files.

@note
[[plplotlibFigure_mod:box]]
[[plplotlibFigure_mod:labels]]
[[plplotlibFigure_mod:ticks]]
[[plplotlibFigure_mod:title]]
[[plplotlibFigure_mod:xlabel]]
[[plplotlibFigure_mod:xticks]]
[[plplotlibFigure_mod:ylabel]]
[[plplotlibFigure_mod:yticks]]

### `lineColor`

A `character` value noting the color to use when painting lines.
Accepted values are the following:

	* 'k', 'black'   :: Black
	* 'w', 'white'   :: White
	* 'r', 'red'     :: Red
	* 'g', 'green'   :: Green
	* 'b', 'blue'    :: Blue
	* 'm', 'magenta' :: Magenta
	* 'y', 'yellow'  :: Yellow
	* 'c', 'cyan'    :: Cyan
	* 'fg'           :: Foreground
	* 'bg'           :: Background

Additionally, the `character` value may contain an ascii decimal encoding of
`real` number between zero and one.
In this case, the color will be taken from the continuous colormap instead
of the discrete indexed colors. For example: `lineColor='  0.534 '` This can
easily be automated through the use of internal files.

@note
[[plplotlib1D_mod:hist]]
[[plplotlib1D_mod:bar]]
[[plplotlib1D_mod:barh]]
[[plplotlib1D_mod:plot]]
[[plplotlib2D_mod:contour]]
[[plplotlib2D_mod:quiver]]
[[plplotlib3D_mod:plot3]]
[[plplotlib3D_mod:wireframe]]

### `lineStyle`

The style of lines can be changed through the `lineStyle` argument which takes
a `character` value. Accepted values are the following:

	* '-'  :: Solid line
	* '--' :: Dashed line
	* ':'  :: Dotted line

@note
[[plplotlib1D_mod:plot]]
[[plplotlib2D_mod:contour]]
[[plplotlib2D_mod:quiver]]
[[plplotlib3D_mod:plot3]]
[[plplotlib3D_mod:surface]]

### `lineWidth`

The width of lines used in an operation can often be set usin the `lineWidth`
argument, with a `real` number multiple of the default line width. For example,
`lineWidth=2.5_wp` will cause lines to be two and a half times thicker than
normal.

@note
[[plplotlibFigure_mod:ticks]]
[[plplotlibFigure_mod:xticks]]
[[plplotlibFigure_mod:yticks]]
[[plplotlib1D_mod:plot]]
[[plplotlib1D_mod:fillBetween]]
[[plplotlib1D_mod:fillBetweenx]]
[[plplotlib1D_mod:hist]]
[[plplotlib1D_mod:bar]]
[[plplotlib1D_mod:barh]]
[[plplotlib2D_mod:contour]]
[[plplotlib2D_mod:quiver]]
[[plplotlib3D_mod:plot3]]

### `markColor`

A `character` value noting the color to use when painting markers or symbols.
Accepted values are the following:

	* 'k', 'black'   :: Black
	* 'w', 'white'   :: White
	* 'r', 'red'     :: Red
	* 'g', 'green'   :: Green
	* 'b', 'blue'    :: Blue
	* 'm', 'magenta' :: Magenta
	* 'y', 'yellow'  :: Yellow
	* 'c', 'cyan'    :: Cyan
	* 'fg'           :: Foreground
	* 'bg'           :: Background

Unlike line colors, marks cannot use the continuous colormap and are thus
restricted to the indexed colors.

@note
[[plplotlib1D_mod:plot]]
[[plplotlib1D_mod:scatter]]
[[plplotlib3D_mod:plot3]]

### `markStyle`

	* '+' :: Plus
	* 'x  :: Times
	* '*' :: Star
	* '.' :: Point
	* 's' :: Square
	* '^' :: Up triangle
	* '<' :: Left triangle
	* 'v' :: Down triangle
	* '>' :: Right triangle

@note
[[plplotlib1D_mod:plot]]
[[plplotlib1D_mod:scatter]]
[[plplotlib3D_mod:plot3]]

### `markSize`

The size of markers can be scaled using the `markSize` argument, which takes
a `real` value multiple of the default maker size. For example, `markSize=1.5_wp`
will scale up the markers by 50% from the default size.

@note
[[plplotlib1D_mod:plot]]
[[plplotlib1D_mod:scatter]]
[[plplotlib3D_mod:plot3]]
