from setuptools import setup, find_packages

setup(
    name='nngmail',
    version='0.1',
    packages=find_packages(),
    include_package_data=True,
    install_requires=[
        'Click',
        'flask',
        'flask-sqlalchemy',
        'google-api-python-client',
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
