from setuptools import setup, find_packages

setup(
    name='nngmail',
    version='0.1',
    packages=find_packages(),
    include_package_data=True,
    install_requires=[
        'Click',
        'python-dateutil',
        'flask',
        'flask-sqlalchemy',
        'google-api-python-client',
        'oauth2client',
        'options',
        'pytest',
        'pyyaml',
        'sqlalchemy',
        'tqdm',
        'vcrpy'
    ],
    entry_points='''
        [console_scripts]
    ''',
)
