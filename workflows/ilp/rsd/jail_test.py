import pwd
import os
import shutil
import tempfile
import stat
import subprocess

jail_dir = tempfile.mkdtemp()
os.chmod(jail_dir, stat.S_IRUSR | stat.S_IWUSR)#etc
#we use several other permission bits here.
#c.f. best practices article
 
jail_etc = os.path.abspath(os.path.join(jail_dir, "etc"))
os.mkdir(jail_etc)
shutil.copy('/etc/resolv.conf', jail_etc)
 
for required_dir in ['/usr/bin/']:
    #use mount -r --bind as a convenient way of giving read-only access
    #to other folders (libraries, etc) required by the jailed process
    mount_point = "%s%s" % (jail_dir, required_dir)
    os.makedirs(mount_point)
    subprocess.call(["sudo", "mount", "-r", "--bind", required_dir, mount_point])

nobody_user = pwd.getpwnam("nobody").pw_uid
 
os.chroot(jail_dir)
os.chdir('/')
os.seteuid(nobody_user)

p = subprocess.Popen(['python', 'rsd.py'])
stdout_str, stderr_str = p.communicate()

print stdout_str
print stderr_str