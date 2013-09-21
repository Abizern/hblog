---
layout: page
title: "cocoa coding conventions"
footer: false
---

## Table of contents

+ [Purpose](#Purpose)
+ [Xcode Project Settings](#ProjectSettings)
  + [Static Analyser](#StaticAnalyser)
  + [Automatic Reference Counting](#ARC)
  + [Unit Testing](#UnitTesting)
  + [Standard Files](#StandardFiles)
  + [Schemes](#Schemes)
  + [Compiler](#Compiler)
+ [Code Style](#CodeStyle)
  + [imports](#imports)
  + [iVars](#iVars)
  + [Properties](#Properties)
  + [Method and Variable Naming](#Naming)
  + [Dot Notatation](#DotNotation)
  + [Method Signatures](#MethodSignatures)
  + [Private Methods](#PrivateMethods)
  + [Empty Methods](#EmptyMethods)
  + [Types](#Types)
    + [integer](#integer)
    + [BOOL](#BOOL)
    + [floats](#floats)
  + [Error Handling](#ErrorHandling)
  + [Static Strings](#StaticStrings)
  + [Enums](#Enums)
  + [TODOs](#TODOs)
+ [Code Layout](#CodeLayout)
  + [#pragma Is Your Friend](#Pragma)
  + [Import Ordering](#ImportOrdering)
  + [Method Ordering](#MethodOrdering)
  + [Unit Test Classes](#UnitTestClasses)
  + [Comments](#Comments)
  + [Horizontal Spacing](#HorizontalSpacing)
  + [Vertical Spacing](#VerticalSpacing)
  + [Braces](#Braces)
  + [Control Structures](#ControlStructures)
    + [if/else](#ifelse)
    + [The Ternary Operator](#Ternary)
    + [Switch](#Switch)
    + [For](#For)
    + [While](#While)
  + [Golden Path](#GoldenPath)
  + [Multiple Returns](#MultipleReturns)
+ [Version Control](#VersionControl)
+ [The Boyscout Rule](#BoyscoutRule)
+ [Source](#Source)
+ [Credits](#Credits)
+ [Footnote](#Footnote)
+ [Revision History](#RevisionHistory)

## <a id="Purpose"></a>Purpose

Consistent coding conventions are one way to keep code readable and
maintainable. Written coding conventions provide a handy reference for the times
when one can't remember what conventions apply.

This is a living document, and if I find that I am breaking the conventions I've
set for myself, then I'll update the document to match what I'm doing. This is
more a record of my choices than a rod for my own back.

In line with George Orwell's last rule in
[Politics and the English Language (1946)](http://www.resort.com/~prime8/Orwell/patee.html)
Break any of these rules sooner than doing anything barbarous.

## <a id="ProjectSettings"></a>Xcode Project Settings

### <a id="StaticAnalyser"></a>Static Analyser

Run the Static Analyser automatically with builds. Strive for zero
warnings and errors with the default warnings and errors turned on. Use the
Clang pragmas to silence warnings where you really have to.

### <a id="ARC"></a>Automatic Reference Counting

All new projects use ARC. The compiler is better placed to insert boilerplate code.

### <a id="UnitTesting"></a>Unit Testing

Use the Unit Testing option when creating new projects from the templates. You
may not always use strict TDD, but when the time comes where something needs to
be tested automatically, rather than running through a series of steps with a
running app, having this set up will be a timesaver.

That said, Unit Testing is a useful discipline, use it where you can. Don't let
the lack of a complete set of tests put you off writing any unit
tests. Something is better than nothing, as long as you remember that the tests
are not comprehensive.

### <a id="StandardFiles"></a>Standard Files

The Application Delegate class is named `AppDelegate`.

The project templates create standard files - a pch file, a main file, a plist
file. Regardless of what the templates are set up to do rename them to
`Prefix.pch`, `main.m` and `Info.plist`. Some changes will need to be made to
the project settings that reference these files to make them work. Similar files
are created for unit testing or other targets, but these need not be renamed;
the simplified names are only be used for the main target of the project.

There are common macros that are used throughout my projects. This header file
should be added to all projects and imported into the `Prefix.pch` file. The most
up to date version is available on Github:

``` objc Common Macros https://gist.github.com/Abizern/325926 View Gist
// Useful Macros.
// The best place to import this is in your project's pch file.
// See http://www.cimgf.com/2010/05/02/my-current-prefix-pch-file/ for details.
// Most current version at https://gist.github.com/325926 along with usage notes.

#ifndef jcscommonmacros
#define jcscommonmacros_1_0  10000
#define jcscommonmacros      jcscommonmacros_1_0
#endif

#ifdef DEBUG
  #define DLog(...) NSLog(@"%s %@", __PRETTY_FUNCTION__, [NSString stringWithFormat:__VA_ARGS__])
  #define ALog(...) [[NSAssertionHandler currentHandler] handleFailureInFunction:[NSString stringWithCString:__PRETTY_FUNCTION__ encoding:NSUTF8StringEncoding] file:[NSString stringWithCString:__FILE__ encoding:NSUTF8StringEncoding] lineNumber:__LINE__ description:__VA_ARGS__]
#else
  #define DLog(...) do { } while (0)
  #ifndef NS_BLOCK_ASSERTIONS
    #define NS_BLOCK_ASSERTIONS
  #endif
  #define ALog(...) NSLog(@"%s %@", __PRETTY_FUNCTION__, [NSString stringWithFormat:__VA_ARGS__])
#endif

#define ZAssert(condition, ...) do { if (!(condition)) { ALog(__VA_ARGS__); }} while(0)

//
// This is a general test for emptiness
// Courtesy of Wil Shipley http://www.wilshipley.com/blog/2005/10/pimp-my-code-interlude-free-code.html

static inline BOOL isEmpty(id thing) {
    return thing == nil
        || ([thing respondsToSelector:@selector(length)]
        && [(NSData *)thing length] == 0)
        || ([thing respondsToSelector:@selector(count)]
        && [(NSArray *)thing count] == 0);
}
```

### <a id="Schemes"></a>Schemes

Xcode creates a standard scheme named after the project for development. As part
of setting up the project create a Release Scheme (and a TestFlight scheme if
appropriate) and set them up to use the correct signing keys. Make these schemes
shared and add them to version control.

### <a id="Compiler"></a>Compiler

Use the most up to date compiler possible and all the associated benefits,
Literals, ARC, auto-synthesis, the static analyser, auto-layout, storyboards, etc.

## <a id="CodeStyle"></a>Code Style

### <a id="imports"></a>imports

See the code layout section to see how to lay out the imports in the .h and .m files

Although the common framework imports should be in the pch file, put all the
required imports into the source files. I find it helps to see what frameworks a
class is set up to deal with.

Only put superclass imports and protocol imports into the header file. Forward
declarations should use the `@class` statement. The remaining headers should go
into the .m file.

### <a id="iVars"></a>iVars

Avoid using iVars, certainly not as part of the public interface. Prefer the use
of properties.

If using iVars treat them as private and add them to a class extension or the
@implementation section if using a compiler that supports it.

``` objective-c
// Bad - declared in the public header

@interface MyClass : NSObject {
    NSString *_aString;
}

// Better - declared in the .m file as part of a class extension

@interface MyClass () {
    NSString *_aString;
}

// Best - declared in the class implementation itself

@implementation MyClass {
    NSString *_aString;
}
```

Note the underscore prefix for iVars. I've gradually come around to the idea of
using them. what they do is a)make clear that you are using an iVar and not a
local varliable, and b)remove the need to rename local variables or parameters
to avoid clashing with iVars.

### <a id="Properties"></a>Properties ###

Prefer immutable to mutable types, and use `copy` not `retain` for types that
are part of a mutable / immutable class cluster unless you **really** need a
mutable type.

The memory management declaration should come first.

Prefer nonatomic to atomic type declarations.

Choose a different getters for `BOOL` types where appropriate.

``` objective-c
@property (strong, nonatomic) UIColor *colour;
@property (copy, nonatomic) NSString *title;
@property (weak, nonatomic) id <DelegateProtocol> delegate;
@property (assign, nonatomic, getter=isDownloading) BOOL downloading;
```

Only expose as much in the public interface as needed. It's easier to have a
smaller surface area of change when working through code, especially during
discovery. If a property needs to be publicly readonly but readwrite for the
class, redeclare the property as readwrite within a class extension.

``` objective-c
// In the .h file
@interface MyClass : NSObject

@property (copy, readonly, nonatomic) NSString *aString;

@end

// In the .m file
@interface MyClass ()

@property (copy, readwrite, nonatomic) NSString *aString;

@end
```

Auto-synthesised properties generate an iVar with a leading underscore. If not
using a compiler that supports auto-synthesis @sythesise backing stores in the
same way.

### <a id="Naming"></a>Method and Variable Naming

Don't fear long descriptive names, Xcode has auto-completion.

Follow the conventions in Apple's
[Coding Guidelines for Cocoa](https://developer.apple.com/library/mac/#documentation/Cocoa/Conceptual/CodingGuidelines/CodingGuidelines.html#//apple_ref/doc/uid/10000146-SW1)

The cocoa frameworks follow an American spelling convention. That means that
there are classes such as `NSColor` and `NSNotificationCenter`. That doesn't
mean that this convention should be followed blindly. Use British spelling:

``` objective-c
// The usual
UIColor *someColor = [UIColor whiteColor];
NSNotificationCenter *notificationCenter = [NSNotificationCenter defaultCenter];

// Better
UIColor *someColour = [UIColor whiteColor];
NSNotificationCenter *notificationCentre = [NSNotificationCenter defaultCenter];
```

The downside to the preffered choice is that there is an inconsistency on the
declaration line and there will be two spellings in use throughout the
code. But, it's more natural for me to write with British Spellings, and let
autocompletion use the correct spellings for the system.

However - overall consistency and project style should take precedence. If this
is your code, your project; then use the British spellings. If you are
contributing to somebody else's code - use whatever is currently in use.

### <a id="DotNotation"></a>Dot Notation

Use it, but don't abuse it.

If it's possible to chain properties together neatly then use it. If the dots
and brackets are becoming interleaved or nested too deep use brackets. This one
is hard to nail down but if you look at the resulting line and you have to pick
apart the calls - change it to all brackets - which should always work.

Only use dot notation for properties.

### <a id="MethodSignatures"></a>Method Signatures

Left aligned, with a space after the scope. There should be a space between the
method segments, and if a parameter is a pointer, then there should be a space
before the asterisk

``` objective-c
// BAD
-(void)doSomethingWithString:(NSString*)aString;

// GOOD
- (void)doSomethingWithString:(NSString *)aString;
```
### <a id="PrivateMethods"></a>Private Methods

These should not be declared in the public interface or a class extension.

Put them at the end of the class implementation in their own `#pragma` section.

### <a id="EmptyMethods"></a>Empty Methods

If there is a method that has no implementation, or is just a call to `super`
then it should be removed.

### <a id="Types"></a>Types

#### <a id="integer"></a>integer

Use `NSInteger` and `NSUInteger` in preference to these. Make sure you use the
standard framework methods with `integer` instead of `int` to get the correct
types back.

For Foundation types, when iterating use `CFIndex`.

#### <a id="BOOL"></a>BOOL

Use `YES` and `NO` not true, false, 1, -1 or anything like that.

Don't compare `BOOL` returns against `BOOL`s:

``` objective-c
// GOOD
if ([self aMethodReturningBOOL]) {

// THE DEVIL'S WORK
if ([self aMethodReturningBOOL] == YES) {
```

#### <a id="floats"></a>floats

Use `CGFloat` instead of `float`.

### <a id="ErrorHandling"></a>Error Handling

Use `NSError` wherever possible. Exceptions and Asserts are used to find
programmer errors. Use the `ALog` and `ZAssert` macros (c.f. Standard Macros)
for convenience.

### <a id="StaticStrings"></a>Static Strings

Use static declarations in preference to #defines.

Use `extern` to declare a public constant that is defined within the class. Use
a simple static otherwise

``` objective-c
// declared in header defined in implementation
// .h
extern NSString * const kMyConstant;

// .m
NSString * const kMyConstant = @"MyConstant";

// For internal use
static NSString *someString;
```

### <a id="Enums"></a>Enums

Use the new typed enumerations.

``` objective-c
typedef enum : unsigned char {
    Red,
    Green,
    Blue
} StandardColours;
```

For simple enumerations there is no need to assign values to the values unless
they are really needed in specific situations. The exception is for bitmask
enumerations which should specify the initial value and the remaining values
should use the shift-left operator (<<).

``` objective-c
typedef enum : NSUInteger {
    ABCOutputNone    = 0,
    ABCOutputSeconds = 1 << 0,
    ABCOutputMinutes = 1 << 1,
    ABCOunpunHours   = 1 << 2
} ABCOutput;
```

### <a id="TODOs"></a>TODOs

Put `// TODO: ` comments to remind yourself of things and use search to find
them.

If you want to mark critical parts that need to be implemented, don't bother
with using build scripts to find the TODOs - just use a `#warning` instead:

``` objective-c
#warning This method needs to be implemented.
```

This will raise a convenient warning with the message when building.

## <a id="CodeLayout"></a>Code layout

### <a id="Pragma"></a>#pragma Is Your Friend

Group methods into related groups and section them up with #pragma
descriptions.

### <a id="ImportOrdering"></a>Import Ordering

See the code style section for what to import and where.

The header files should have the minimal number of imports, as forward declarations are
preferred. In any case  this is the way to lay them out.

- The interface header (i.e. the .h file for the current .m file)
- Framework headers (These should also be in the .pch file).
- A blank line
- The other imports.

If a suitable tool is available the Framework headers and the other imports may be
ordered alphabetically within each of these groups, but it's not often worth the
effort, unless you are looking for some boyscouting to do.

### <a id="MethodOrdering"></a>Method Ordering

- Class methods.
- Instance methods.
  - initialisers.
  - dealloc (if needed).
  - For controllers - their common methods: i.e. view lifecycle methods for view
    controllers, table view delegate and datasource methods for table view
    controllers, etc.
  - Super class overrides.
  - Public methods declared in the header file.
  - Action methods declared in the header file.
  - Custom accessors (setters and/or getters) for declared properties that you
    don't want to leave to auto-synthesize.
  - Other delegate and protocol methods that the class implements.
  - Private methods for the class's own internal use.

Try and keep the order of the methods in the header file the same as in the
implementation file. This is a handy thing to look for when you want to follow
the Boyscout Rule.

### <a id="UnitTestClasses"></a>Unit Test Classes

The Xcode templates create these with a .h and a .m file. In almost all cases this isn't
needed. Unit tests are self contained and having a .h file which can be shared just
increased complexity. So, for unit test classes - put the interface and implementation
in the same .m file for each test class.

### <a id="Comments"></a>Comments

One-line comments `//` are preferred to multiline `/**/` comments. There should
be one space between the comment marker and the comment text.

Comments should be used to document **why** something is happening and not what
is happening and should be used to explain situations that are not trivial.

If you need to use a comment to explain what a variable does - then it's likely
that you haven't used a descriptive enough name for it. If there is a comment to
explain a section of code in a method, consider moving that section of code to a
well named private method instead.

For writing inline documentation use [AppleDoc](http://gentlebytes.com/appledoc/).

If you can't be bothered to keep the comments updated don't have them at
all. Bad or out of date comments are worse than none at all.

### <a id="HorizontalSpacing"></a>Horizontal Spacing

Use 4 spaces to indent code, not tabs.

Long lines aren't a problem. Let Xcode autowrap them. If you do decide to wrap
lines containing methods - make sure the colons are aligned.

### <a id="VerticalSpacing"></a>Vertical Spacing

Use a single blank line between methods. There is no need to leave a blank line
at the start of a method definition.

Use two blank spaces for organisation at a large scale, such as to separate
static definitions from implementation blocks, or to separate the end of the
@property declarations from the method declarations in a header file.

Group lines of codes together when they are related. Think of it like writing,
where sentences that relate to a topic are grouped into paragraphs.

### <a id="Braces"></a>Braces

A brace is always opened on the same line after a space (the exceptions is block
definitions). The closing brace is on a separate line and indented to the same
level as the line with the opening brace.

``` objective-c
- (BOOL)someMethod {
    return YES;
}
```

There should **always** be a space after the control structure (i.e. `if`,
`else`, etc). The `else` and `else if` statements should be on the same line as
the closing brace of the `if` statement.

Xcode is inconsistent about the opening brace, and some of your colleagues may
be too. I wrote a small ruby gem called
[fixbraces](http://abizern.org/fixbraces/) which will correctly position the
braces on the opening line.

### <a id="ControlStructures"></a>Control Structures

See the section on the Golden Path for more usage guides with control
structures.

I originally specified no blank lines between clauses, but I've been turned
around on this. When you are looking at a block of code, the blank link makes
the changed indentation level more obvious.

#### <a id="ifelse"></a>if/else

``` objective-c
if (button.enabled) {
    // do something

} else if (otherButton.enabled) {
    // do something else

} else {
    // do something by default

}
```

Always use a multiline if/else statement with the clauses in braces. This is not
only clearer to read but easier to make changes to.

#### <a id="Ternary"></a>The Ternary operator

A handy structure, but should be used sparingly where it aids readability.

``` objective-c
// Acceptable
NSString *boolString = nil;

if (someBool) {
    boolString = @"YES";

} else {
    boolString = @"NO";

}

DLog(@"The BOOL value is %@", boolString);

// Better. Succinct and readable
DLog(@"The BOOL value is %@", someBool ? @"YES" : @"NO");
```

#### <a id="Switch"></a>Switch

Brackets don't need to be added around each case statement. In some cases, when
using ARC and declaring varibles within a case block an error may be thrown
"Switch case is in protected scope" and then surrounding the case statement with
brackets should clear up the error.

The `default` case should **always** be the last case and should **always** be included.

```objective-c
switch (something.state) {
    case 0: {
        // braced block where required by the compiler
        break;
    }

    case 1:
        // Something
        break;

    case 2:

    case 3:
        // Something
        break;

    default:
        // Something

}
```

#### <a id="For"></a>For

```objective-c
for (NSInteger idx = 0; idx < 10; idx++) {
    // Do something
}

for (NSString *key in dictionary) {
    // Do something
}
```

When iterating using integers, it is preferred to start at `0` and use `<`
rather than starting at `1` and using `<=`. Also, use `idx` instead of `i`. Fast
enumeration is generally preferred because it is easier on the eyes as well as
faster. Also consider using block enumeration where applicable.

#### <a id="While"></a>While

```objective-c
while (something < somethingElse) {
    // Do something
}
```

### <a id="GoldenPath"></a>Golden Path

When using conditionals, the left hand margin of the code should be the
**golden** or **happy path**. This is the part of the code that should be
commonly executed.  For example, the common way of writing initialisers doesn't
follow this:

``` objective-c
// BAD
- (id)init {
    self = [super init];
   if (self) {
        // initialisations here

    }

    return self;
}

// GOOD
- (id)init {
    if (!(self = [super init])) {
        return nil; // Bail!

    }

    // initialisations here
    return self;
}
```

This keeps the main part of the code furthest to the left, with the exceptional
conditions further to the right.

### <a id="MultipleReturns"></a>Multiple Returns

Purists say that there should be only one return point from a function. There
are times where multiple returns are not a problem. Look at the example
initialiser above. The good example contains multiple returns values - in the
case where the super class initialiser returns the nil, the method returns a nil
straight away. I find that this makes it clearer when reading the code that the
function will return at this point and I don't have to look through the method
any more to see if the value of self is changed later on before returning.

## <a id="VersionControl"></a>Version Control

You have no excuse for not using version control.

Aim for all commits on the master branch to compile cleanly and work (for some
definition of 'work').

Develop on branches. Commit early and commit often, don't be afraid to rebase
commits. Unlike the master branch, these don't all have to compile
cleanly. You'll be rebasing them onto master anyway so they'll be tidied up then.

When writing commit messages use the correct
[conventions](http://365git.tumblr.com/post/3308646748/writing-git-commit-messages)

## <a id="BoyscoutRule"></a>The Boyscout Rule

The Boyscouts say: "Always leave a campsite cleaner than you found it". A similar
rule is said to apply to programming - "Always check in a module cleaner than
you checked it out".

In these days of distributed version control, I'd say that means if you see a
problem in code - fix it. Don't leave it for later, don't leave it for someone
else. Version control, unit tests, comments are all structures
that support making changes like this.

## <a id="Source"></a>Source

If you feel like creating your own, you can grab the latest markdown for this
page from [Github](https://github.com/Abizern/abizern.github.com/blob/source/source/cocoa-coding-conventions/index.markdown).

## <a id="Credits"></a>Credits

The basis for these standards were taken from

+ Zarra Studios'
[ZDS Code Style Guide](http://www.cimgf.com/zds-code-style-guide/)
+ NOUSguide's
[Coding-Conventions](http://github.com/NOUSguide/Coding-Conventions)

## <a id="Footnote"></a>Footnote

I realise that these conventions are not to everyones' tastes, and there is no
comment section on this blog for you to tell me how wrong I am. It's okay for
you to disagree with me. These are **my** conventions and I encourage you to use
this as a template to create and evolve your own guidelines.

But, my contact details are on the about page if you do want to give me a piece
of your mind. I'm not above changing my opinion about these.

## <a id="RevisionHistory"></a>Revision History

Last updated April 20, 2013

Full history
[available on GitHub](https://github.com/Abizern/abizern.github.com/commits/source/source/cocoa-coding-conventions
 "Commit history for the file").
