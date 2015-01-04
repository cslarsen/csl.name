# Bootstrap Liquid Tags
# #
# Copyright (C) 2015 Christian Stigen Larsen
# Licensed under the Affero GPL v3 or later.

module Bootstrap

  # Prints a callout box with an optional title and Markdown or HTML contents.
  # You need to use Bootstrap for this to work, and you need to install the
  # additional CSS from http://cpratt.co/twitter-bootstrap-callout-css-styles/
  # as well.
  #
  # How to use it:
  #
  #   {% callout warning %}
  #   Hello, this is a warning!
  #   We used the style `bs-callout-warning` here.
  #   {% endcallout %}
  #
  #   {% callout danger: Oops! %}
  #   This box will make "Oops!" a title header.
  #   {% endcallout %}
  #
  # Supported callout types:
  #
  #   - default (grey)
  #   - primary (dark blue)
  #   - success (green)
  #   - info (light blue)
  #   - warning (yellow)
  #   - danger (red)
  #
  class Callout < Liquid::Block
    def initialize(tag_name, params, tokens)
      super
      @type, @title = params.split(":")
      @type = "bs-callout-" + @type
    end

    def render(context)
      content = super
      html = Kramdown::Document.new(content).to_html

      title = ""
      if @title
        title = "\n<h4>" + @title.strip + "</h4>\n"
      end

      "<div class='bs-callout #{@type}'>" + title + html + "</div>"
    end
  end

  # Produces a Bootstrap lead div tag that supports Markdown or HTML contents.
  #
  # Usage:
  #
  #   {% lead %}
  #   Four score and seven years ago ...
  #   {% endlead %}
  #
  class Lead < Liquid::Block
    def initialize(name, whatever, tokens)
      super
    end

    def render(context)
      content = super
      html = Kramdown::Document.new(content).to_html
       "<div class='lead'>" + html + "</div>"
      end
  end

end # module Bootstrap

Liquid::Template.register_tag('callout', Bootstrap::Callout)
Liquid::Template.register_tag('lead', Bootstrap::Lead)
