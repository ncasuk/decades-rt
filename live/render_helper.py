import os
#templating
from jinja2 import Environment,FileSystemLoader
#Standard python modules for config and date/time functions
from datetime import datetime, timedelta
from pytz import timezone

def render_template(template_name, **context):
    extensions = context.pop('extensions', [])
    globals = context.pop('globals', {})

    jinja_env = Environment(
            loader=FileSystemLoader(os.path.join(os.path.dirname(__file__), 'templates')),
            extensions=extensions,
            )
    jinja_env.filters['latitude'] = latitude
    jinja_env.filters['longitude'] = longitude
    jinja_env.filters['humanise_timestamp'] = humanise_timestamp
    jinja_env.globals.update(globals)

    #jinja_env.update_template_context(context)
    return jinja_env.get_template(template_name).render(context)

def latitude(value):
   value=float(value)
   return ("%.2f" % abs(value)) + ('S' if value <0 else 'N')

def longitude(value):
   value=float(value)
   return ("%.2f" % abs(value)) + ('W' if value <0 else 'E')

def humanise_timestamp(value):
   try:
      return datetime.fromtimestamp(value,timezone('utc')).strftime('%Y-%m-%d %H:%M:%S %Z')
   except TypeError:
      return None
