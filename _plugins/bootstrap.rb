# Bootstrap Liquid Tags
# #
# Copyright (C) 2015 Christian Stigen Larsen
# Licensed under the Affero GPL v3 or later.

module Bootstrap

  # Usage:
  #
  #   {% callout warning %}
  #   ...
  #   {% endcallout %}
  #
  # You can install the CSS from
  # http://cpratt.co/twitter-bootstrap-callout-css-styles/
  #
  class CalloutBeginTag < Liquid::Tag
    def initialize(tag_name, callout_type, tokens)
      super
      @type = "bs-callout-" + callout_type
    end

    def render(context)
      "<div class='bs-callout #{@type}'>"
    end
  end

  class CalloutEndTag < Liquid::Tag
    def initialize(tag_name, wut, tokens)
      super
    end

    def render(context)
      "</div>"
    end
  end

end

Liquid::Template.register_tag('callout', Bootstrap::CalloutBeginTag)
Liquid::Template.register_tag('endcallout', Bootstrap::CalloutEndTag)
