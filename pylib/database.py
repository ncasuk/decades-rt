import psycopg2, psycopg2.extensions
def get_database():
    conn = psycopg2.connect (host = "localhost",
                           user = "inflight",
                           password = "wibble",
                           database = "inflightdata")
    #turn off transactions so the incoming INSERTS do not interfere with each other
    conn.set_isolation_level(psycopg2.extensions.ISOLATION_LEVEL_AUTOCOMMIT) 
    return conn
