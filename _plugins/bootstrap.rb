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
  class Callout < Liquid::Block
    def initialize(tag_name, callout_type, tokens)
      super
      @type = "bs-callout-" + callout_type
    end

    def render(context)
      content = super
      html = "#{Kramdown::Document.new(content).to_html}"
      "<div class='bs-callout #{@type}'>" + html + "</div>"
    end
  end

  class Lead < Liquid::Block
    def initialize(name, whatever, tokens)
      super
    end

    def render(context)
      content = super
      html = "#{Kramdown::Document.new(content).to_html}"
       "<div class='lead'>" + html + "</div>"
      end
  end

end # module Bootstrap

Liquid::Template.register_tag('callout', Bootstrap::Callout)
Liquid::Template.register_tag('lead', Bootstrap::Lead)
