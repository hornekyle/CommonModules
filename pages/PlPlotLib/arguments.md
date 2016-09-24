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
[[plplotlib_mod:box]]
[[plplotlib_mod:labels]]
[[plplotlib_mod:ticks]]
[[plplotlib_mod:title]]
[[plplotlib_mod:xlabel]]
[[plplotlib_mod:xticks]]
[[plplotlib_mod:ylabel]]
[[plplotlib_mod:yticks]]

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
[[plplotlib_mod:hist]]
[[plplotlib_mod:bar]]
[[plplotlib_mod:barh]]
[[plplotlib_mod:plot]]
[[plplotlib_mod:plot3]]
[[plplotlib_mod:contour]]
[[plplotlib_mod:wireframe]]
[[plplotlib_mod:quiver]]

### `lineStyle`

The style of lines can be changed through the `lineStyle` argument which takes
a `character` value. Accepted values are the following:

	* '-'  :: Solid line
	* '--' :: Dashed line
	* ':'  :: Dotted line

@note
[[plplotlib_mod:plot]]
[[plplotlib_mod:plot3]]
[[plplotlib_mod:contour]]
[[plplotlib_mod:surface]]
[[plplotlib_mod:quiver]]

### `lineWidth`

The width of lines used in an operation can often be set usin the `lineWidth`
argument, with a `real` number multiple of the default line width. For example,
`lineWidth=2.5_wp` will cause lines to be two and a half times thicker than
normal.

@note
[[plplotlib_mod:ticks]]
[[plplotlib_mod:xticks]]
[[plplotlib_mod:yticks]]
[[plplotlib_mod:hist]]
[[plplotlib_mod:plot]]
[[plplotlib_mod:plot3]]
[[plplotlib_mod:contour]]
[[plplotlib_mod:quiver]]
[[plplotlib_mod:bar]]
[[plplotlib_mod:barh]]
[[plplotlib_mod:fillBetween]]
[[plplotlib_mod:fillBetweenx]]

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
[[plplotlib_mod:scatter]]
[[plplotlib_mod:plot]]
[[plplotlib_mod:plot3]]

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
[[plplotlib_mod:scatter]]
[[plplotlib_mod:plot]]
[[plplotlib_mod:plot3]]

### `markSize`

The size of markers can be scaled using the `markSize` argument, which takes
a `real` value multiple of the default maker size. For example, `markSize=1.5_wp`
will scale up the markers by 50% from the default size.

@note
[[plplotlib_mod:scatter]]
[[plplotlib_mod:plot]]
[[plplotlib_mod:plot3]]
