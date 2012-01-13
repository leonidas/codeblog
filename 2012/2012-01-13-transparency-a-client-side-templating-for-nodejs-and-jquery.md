# Transparency - Client-side templating for Node.js and jQuery

[Single-page web applications][1] have been pretty much standard for quite a while, and I'm a strong advocate for 
numerous reasons (smaller latency, separation of concerns and ease of testing to name a few).

However, one point I haven't felt too good about is client side rendering. It's just silly how cumbersome it is to 
compile the template, render the data and finally manipulate the DOM. For example, with popular template engines like 
[Handlebars][2] or [Mustache][3], you typically need do something like

```
<script id="entry-template" type="text/x-handlebars-template">
  <div class="entry">
    <h1>{{title}}</h1>
    <div class="body">
      {{body}}
    </div>
  </div>
</script>
```

```javascript
var data     = {title: "My New Post", body: "This is my first post!"}
var source   = $("#entry-template").html();
var template = Handlebars.compile(source);
var html     = template(data);
$('container').empty().append(html);
```

Frustrated with the amount of labor, I decided to roll out my own and focus on simplicity. In this article, 
I walk through some of the main design decisions and corresponding implementation.

## No syntax, please!

I started with a modest goal: Given I have a static web page

```html
<div class="container">
  <div class="hello"></div>
</div>
```

and a simple JavaScript object

```javascript
data = {
  hello: "Hi There!"
};
```

I want to render that on object on the page with a single function call. No template definition in script tags, 
no extra markup, no manual DOM manipulation. So, when I call `$('.container').render(data);`, I should see the 
following in the browser

```html
<div class="container">
  <div class="hello">Hi There!</div>
</div>
```

We'll, it turned out, that wasn't too hard to implement. DOM manipulation is the bread and butter of jQuery, 
so all we need to do is

1. Iterate over the key-value pairs of the javascript objects
2. Render the value on the matching DOM element.

The initial implementation looked like something like this:

```coffeescript
jQuery.fn.render = (data) ->
  template = this

  for key, value of data
    for node in template.find(".#{key}")
      node     = jQuery(node)
      children = node.children().detach()
      node.text value
      node.append children
```

## There are too many loops out there

The next logical step was support for collections. I wanted to keep the interface exactly the same, without explicit 
loops or partials. Given an object like

```javascript
friends = [
  {name: "Al Pacino"},
  {name: "The Joker"}
]
```

And a web page like

```html
<ul class="container">
  <li class="name"></li>
</ul>
```

When I call `$('.container').render(friends)`, I should see

```html
<ul class="container">
  <li class="name">Al Pacino</li>
  <li class="name">The Joker</li>
</ul>
```

Obviously, we need to extend the existing implementation with following steps

1. Iterate through the list of data objects
2. Take a new copy of the template for each object
3. Append  the result to the DOM

```coffeescript
jQuery.fn.render = (data) ->
  template = this.clone()
  context  = this
  data     = [data] unless jQuery.isArray(data)
  context.empty()

  for object in data
    tmp = template.clone()

    for key, value of data
      for node in tmp.find(".#{key}")
        node     = jQuery(node)
        children = node.children().detach()
        node.text value
        node.append children

    context.append tmp.children()
```

It's worth noticing, that the rendering a single object is actually just an edge case of rendering a list of data 
objects. That gives us an opportunity to generalize the edge case by encapsulating the single object into a list as 
shown above.

## Do it again!

The previous implementation works, kind of. However, if you call `$('container').render(friends)` twice, it fails.

Result after the first call

```html
<ul class="container">
  <li class="name">Al Pacino</li>
  <li class="name">The Joker</li>
</ul>
```

Result after the second call

```html
<ul class="container">
  <li class="name">Al Pacino</li>
  <li class="name">Al Pacino</li>
  <li class="name">The Joker</li>
  <li class="name">The Joker</li>
</ul>
```

The reason is obvious. The current implementation finds two matching elements on the second call and renders 
the name on the both elements. That sucks, because it means you'd have to manually keep the original templates in safe.

To avoid the problem, we need to

1. Cache the original template on the first .render()
2. Use the cached template on the successive calls

Luckily, thanks to jQuery `data()`, the functionality is trivial to implement.

```coffeescript
jQuery.fn.render = (data) ->
  context  = this
  data     = [data] unless jQuery.isArray(data)
  context.data('template', context.clone()) unless context.data 'template'
  context.empty()

  for object in data
    template = context.data('template').clone()

    # Render values
    for key, value of data
      for node in tmp.find(".#{key}")
        node     = jQuery(node)
        children = node.children().detach()
        node.text value
        node.append children

    context.append template.children()
```

## My way or the highway

Rails has shown us how powerful it is to have strong conventions over configurations. Sometimes, however, you need to 
do the things differently and then it helps to have all the power. In JavaScript, that means functions.

I wanted to be able to hook into rendering and define by functions how the rendering should happen. Common scenarios 
would include, e.g., decorators and attribute assignment.

For example, given a template

```html
<div class="container">
  <div class="name"></div>
</div>
```

I want be able render the following data object with the directive

```coffeescript
person = {
  firstname: "Lucy",
  lastname:  "Lambda"
}

directives =
  name: -> "#{@firstname} #{@lastname}"

$('.container').render person, directives
```

And the result should be

```html
<div class="container">
  <div class="name">Lucy Lambda</div>
</div>
```

At first, implementing directives might seem like a daunting task, but given the flexibility and and power of 
javascript functions and object literals, it isn't that bad. We only need to

1. Iterate over the key-function pairs of the directive object
2. Bind the function to the data object and execute it
3. Assign the return value to the matching DOM element

```coffeescript
jQuery.fn.render = (data, directives) ->
  context  = this
  data     = [data] unless jQuery.isArray(data)
  context.data('template', context.clone()) unless context.data('template')
  context.empty()

  for object in data
    template = context.data('template').clone()

    # Render values
    for key, value of data
      for node in tmp.find(".#{key}")
        renderNode node, value

    # Render directives
    for key, directive of directives
      for node in template.find(".#{key}")
        renderNode node, directive.call(object, node)

    context.append template.children()

renderNode = (node, value) ->
  node = jQuery(node)
  children = node.children().detach()
  node.text value
  node.append children
```

## Generalizing to nested data

We'll, I bet you saw this coming. Why stop here, if we could easily support nested objects, lists and directives. 
For each child object, we should do exactly same operations that we did for the parent object. Sounds like recursion 
and, indeed, we need to add only couple of lines:

```coffeescript
jQuery.fn.render = (data, directives) ->
  context  = this
  data     = [data] unless jQuery.isArray(data)
  context.data('template', context.clone()) unless context.data('template')
  context.empty()

  for object in data
    template = context.data('template').clone()

    # Render values
    for key, value of data when typeof value != 'object'
      for node in tmp.find(".#{key}")
        renderNode node, value

    # Render directives
    for key, directive of directives when typeof directive == 'function'
      for node in template.find(".#{key}")
        renderNode node, directive.call(object, node)

    # Render children
    for key, value of object when typeof value == 'object'
      template.find(".#{key}").render(value, directives[key])

    context.append template.children()

renderNode = (node, value) ->
  node = jQuery(node)
  children = node.children().detach()
  node.text value
  node.append children
```

## Using Transparency in the real world applications

Writing Transparency has been a delightful experience. It gave me a chance to get my feet wet with node.js, 
CoffeeScript, Jasmine and jQuery plugin development. At Leonidas, we've used it in a numerous projects in the past 
couple of months, and so far, we've been happy with it.

The actual implementation is 66 lines of CoffeeScript and the sources are available at [GitHub][5]. To make it easy to 
try, we've set up [Transparency demo site][4]. Enjoy!

Cheers,

Jarno Keskikangas <[jarno.keskikangas@leonidasoy.fi](mailto://jarno.keskikangas@leonidasoy.fi)>

Leonidas Oy <[http://leonidasoy.fi](http://leonidasoy.fi)>


[1]: http://en.wikipedia.org/wiki/Single-page_application
[2]: http://handlebarsjs.com/
[3]: http://mustache.github.com/
[4]: http://leonidas.github.com/transparency
[5]: http://github.com/leonidas/transparency