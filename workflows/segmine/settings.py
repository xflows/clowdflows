import os

package_name = 'segmine'

package_root = os.path.dirname(__file__)
package_statics = os.path.join(os.path.dirname(__file__), 'static', package_name)
package_bin = os.path.join(package_root, 'bin')

auto_update_db = False
create_backups = True

