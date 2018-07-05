import dateutil.parser as dtparse
import importlib
import json
import os
import pytest

from sqlalchemy import Table

from nngmail import app, db
from nngmail.models import AddresseeEnum

@pytest.fixture
def client():
    app.config['SQLALCHEMY_DATABASE_URI'] = 'sqlite:///:memory:'
    app.config['TESTING'] = True
    client = app.test_client()

    with app.app_context():
        db.create_all()

    yield client

def load_fixtures(filepath):
    enum_map = {}
    for tt in AddresseeEnum:
        enum_map[tt.value] = tt

    def _datetime_parser(dct):
        for key, value in list(dct.items()):
            try:
                dct[key] = dtparse.parse(value)
            except Exception:
                pass
            if key == 'type_':
                dct[key] = enum_map[value]
        return dct

    with open(filepath, 'rb') as fin:
        fixtures = json.load(fin, object_hook=_datetime_parser)

    conn = db.engine.connect()
    metadata = db.metadata

    for fixture in fixtures: 
        if 'model' in fixture:
            module_name, class_name = fixture['model'].rsplit('.', 1)
            module = importlib.import_module(module_name)
            model = getattr(module, class_name)
            for fields in fixture['records']:
                obj = model(**fields)
                db.session.add(obj)
            db.session.commit()
        elif 'table' in fixture:
            table = Table(fixture['table'], metadata)
            conn.execute(table.insert(), fixture['records'])
        else:
            raise ValueError("Fixture missing a 'model' or 'table' field: "
                             "{0}".format(json.dumps(fixture)))

    
@pytest.fixture
def fixtures(client):
    fixtures_dirs = [os.path.join(app.root_path, 'tests/fixtures'),
                     os.path.join(app.root_path, 'api/tests/fixtures')]
    for directory in app.config.get('FIXTURES_DIRS', []):
        if not os.path.isabs(directory):
            directory = os.path.abspath(os.path.join(app.root_path, directory))
        fixtures_dirs.append(directory)

    for filename in ['db-fixtures.json']:
        for directory in fixtures_dirs:
            print(directory)
            filepath = os.path.join(directory, filename)
            if os.path.exists(filepath):
                # TODO load the data into the database
                load_fixtures(filepath)
                break
        else:
            raise IOError("Error loading '{0}'. "
                          "File could not be found".format(filename))

    
