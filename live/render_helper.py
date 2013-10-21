import os
#templating
from jinja2 import Environment,FileSystemLoader
def render_template(template_name, **context):
    extensions = context.pop('extensions', [])
    globals = context.pop('globals', {})

    jinja_env = Environment(
            loader=FileSystemLoader(os.path.join(os.path.dirname(__file__), 'templates')),
            extensions=extensions,
            )
    jinja_env.filters['latitude'] = latitude
    jinja_env.filters['longitude'] = longitude
    jinja_env.globals.update(globals)

    #jinja_env.update_template_context(context)
    return jinja_env.get_template(template_name).render(context)

def latitude(value):
   value=float(value)
   return ("%.2f" % abs(value)) + ('S' if value <0 else 'N')

def longitude(value):
   value=float(value)
   return ("%.2f" % abs(value)) + ('W' if value <0 else 'E')

