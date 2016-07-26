from django.db.models import ImageField
from django.db.models.fields.files import ImageFieldFile
from PIL import Image
from django.core.files.base import ContentFile
import cStringIO

#za south
#from south.modelsinspector import add_introspection_rules

def generate_thumb(img, thumb_size, format):
    """
    Generates a thumbnail image and returns a ContentFile object with the thumbnail
    
    Parameters:
    ===========
    img         File object
    
    thumb_size  desired thumbnail size, ie: (200,120)
    
    format      format of the original image ('jpeg','gif','png',...)
                (this format will be used for the generated thumbnail, too)
    """
    
    img.seek(0) # see http://code.djangoproject.com/ticket/8222 for details
    image = Image.open(img)
    
    # Convert to RGB if necessary
    if image.mode not in ('L', 'RGB', 'RGBA'):
        image = image.convert('RGB')
        
    # get size
    thumb_w, thumb_h = thumb_size
    # If you want to generate a square thumbnail
    if thumb_w == thumb_h:
        # quad
        xsize, ysize = image.size
        # get minimum size
        minsize = min(xsize,ysize)
        # largest square possible in the image
        xnewsize = (xsize-minsize)/2
        ynewsize = (ysize-minsize)/2
        # crop it
        image2 = image.crop((xnewsize, ynewsize, xsize-xnewsize, ysize-ynewsize))
        # load is necessary after crop                
        image2.load()
        # thumbnail of the cropped image (with ANTIALIAS to make it look better)
        image2.thumbnail(thumb_size, Image.ANTIALIAS)
    else:
        # not quad
        image2 = image
        image2.thumbnail(thumb_size, Image.ANTIALIAS)
    
    io = cStringIO.StringIO()
    # PNG and GIF are the same, JPG is JPEG
    if format.upper()=='JPG':
        format = 'JPEG'
    
    image2.save(io, format)
    return ContentFile(io.getvalue())

class ThumbnailFieldFile(ImageFieldFile):
    def __init__(self, *args, **kwargs):
        super(ThumbnailFieldFile, self).__init__(*args, **kwargs)
                  
    def save(self, name, content, save=True):
        super(ThumbnailFieldFile, self).save(name, content, save)
        
        if self.field.size:
            name = self.name
            size = self.field.size
            split = self.name.rsplit('.',1)
            
            thumb_content = generate_thumb(content, size, split[1])
            self.storage.delete(name)
            self.storage.save(name, thumb_content)
        
    def delete(self, save=True):
        super(ThumbnailFieldFile, self).delete(save)
        
class ThumbnailField(ImageField):
    attr_class = ThumbnailFieldFile
    def __init__(self, verbose_name=None, name=None, width_field=None, height_field=None, size=None, **kwargs):
        self.verbose_name=verbose_name
        self.name=name
        self.width_field=width_field
        self.height_field=height_field
        self.size = size
        super(ImageField, self).__init__(**kwargs)
        
#south introspection
# add_introspection_rules([], ["^workflows\.thumbs\.ThumbnailField"])