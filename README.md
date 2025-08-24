# MBE

## Name

'MBE' stands for 'Madf's Blog Engine'.

## How to build and run

### Stack

```
# stack build
# stack run
```

### Cabal

```
```

### Important files and dirs

 * `/etc/mbe/config.ini` - the config file;
 * `/var/lib/mbe/storage.db` - the database;
 * `/var/lib/mbe/key.jks` - the key used for JWT;
 * `/var/www/<your-domain-name>` - the static file location;
 * `/var/www/<your-domain-name>/static` - engine JS and CSS.

All paths are recomended but are configurable in `config.ini`. The path to the `config.ini` can be supplied to the binary.

## Links

[Design considerations](design-considerations.md)
