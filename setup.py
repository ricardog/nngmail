from setuptools import setup, find_packages

setup(
    name='nnserver',
    version='0.1',
    packages=find_packages(),
    include_package_data=True,
    install_requires=[
        'Click',
        'google-api-python-client',
        'options',
        'pyyaml',
        'sqlalchemy',
        'tqdm'
    ],
    entry_points='''
        [console_scripts]
        nnserver=scripts.nnserver:daemon
    ''',
)
