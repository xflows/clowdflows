from django import template

register = template.Library()

def paginator(context, adjacent_pages=2):
    """
    To be used in conjunction with the object_list generic view.

    Adds pagination context variables for use in displaying first, adjacent and
    last page links in addition to those created by the object_list generic
    view.

    Required context variables: paged: The Paginator.page() instance.

    """
    paged = context['paged'] # the paginator.page(page) instance
    paginator = paged.paginator
    startPage = max(paged.number - adjacent_pages, 1)
    if startPage <= 3: startPage = 1
    endPage = paged.number + adjacent_pages + 1
    if endPage >= paginator.num_pages - 1: endPage = paginator.num_pages + 1
    page_numbers = [n for n in range(startPage, endPage) \
            if n > 0 and n <= paginator.num_pages]

    return {
        'paged': paged,
        'paginator': paginator,
        'page': paged.number,
        'pages': paginator.num_pages,
        'page_numbers': page_numbers,
        'next': paged.next_page_number(),
        'previous': paged.previous_page_number(),
        'has_next': paged.has_next(),
        'has_previous': paged.has_previous(),
        'show_first': 1 not in page_numbers,
        'show_last': paginator.num_pages not in page_numbers,
        'last':paginator.num_pages,
    }

register.inclusion_tag('include/paginator.html', takes_context=True)(paginator)

"""
Example template:

<div class="pager">
   {% if has_previous %}
      <span class="page">
      <a href="?page={{ previous }}">&lt; Prev</a>
      </span>
   {% endif %}

   {% if show_first %}
      <span class="page"><a href="?page=1">1</a></span>
      <span class="ellipsis">...</span>
   {% endif %}
   {% for linkpage in page_numbers %}
      {% ifequal linkpage page %}
         <span class="current">{{ page }}</span>
      {% else %}
         <span class="page"><a href="?page={{ linkpage }}"
               >{{ linkpage }}</a></span>
      {% endifequal %}
   {% endfor %}
   {% if show_last %}
      <span class="ellipsis">...</span>
      <span class="page"><a href="?page=last">{{ pages }}</a></span>
   {% endif %}
   {% if has_next %}
      <span class="page"><a href="?page={{ next }}">Next &gt;</a></span>
   {% endif %}
</div>

"""