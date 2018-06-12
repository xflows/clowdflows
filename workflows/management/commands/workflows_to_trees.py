from django.core.management.base import BaseCommand, CommandError
from workflows.models import Category, AbstractWidget, AbstractInput, AbstractOutput, AbstractOption, Workflow, Input, Output
import random, collections
import cPickle as pickle
from django.conf import settings


class Command(BaseCommand):
    help = 'Convert all workflows to trees that can be used in genetic algorithms'

    def add_arguments(self, parser):
        super(add_arguments)

    def handle(self, *args, **options):
        workflows_to_trees()



class Tree:

    def __init__(self, name):
        self.first_widgets = []
        self.all_connected_widgets = []
        self.all_unconnected_widgets = []
        self.last_widgets = [] 
        self.name = name


    def add_node(self, node):
        #print('unconnected widgets: ', node, self.all_unconnected_widgets)
        if not node.input:
            self.first_widgets.append(node)
            self.all_connected_widgets.append(node)
            for widget in self.all_unconnected_widgets:
                self.add_node(widget)
        else:
            added_connections = []
            for widget in self.all_connected_widgets:
                for pre_id in  node.previous_widgets_id:
                    if pre_id == widget.widget_id:
                        node.previous_list.append(widget)
                        widget.next_list.append(node)
                        if node not in self.all_connected_widgets:
                            self.all_connected_widgets.append(node)
                        added_connections.append(pre_id)
            node.previous_widgets_id = [pre_id for pre_id in node.previous_widgets_id if pre_id not in added_connections]

            if len(node.previous_widgets_id) > 0:
                if node not in self.all_unconnected_widgets:
                    #print(self.all_unconnected_widgets)
                    self.all_unconnected_widgets.append(node)
            else:
                if node in self.all_unconnected_widgets:
                    self.all_unconnected_widgets.remove(node)
            if len(added_connections) > 0:
                for widget in self.all_unconnected_widgets:
                    self.add_node(widget)
				    
        if not node.output:
            self.last_widgets.append(node)

    
    def get_previous_widgets(self, node, previous_widgets = []):
        for previous_node in node.previous_list:
            previous_widgets.append(previous_node)
            return self.get_previous_widgets(previous_node, previous_widgets)
        return previous_widgets


    def get_next_widgets(self, node, next_widgets = []):
        for next_node in node.next_list:
            next_widgets.append(next_node)
            return self.get_next_widgets(next_node, next_widgets)
        return next_widgets


    def print_tree(self):
        for w in self.all_connected_widgets:
            print(str(w) +  ' is between: ', w.previous_list, ' and ', w.next_list)
        for w in self.all_unconnected_widgets:
            print('All unconnected widgets:', w.name)
        for w in self.first_widgets:
            print('First widgets:', w.name)



        
    
class Node:

    def __init__(self, widget_id, abstract_id, name, category, inputs, outputs, previous_widgets_id, next_list=[]):
        self.widget_id = widget_id
        self.abstract_id = abstract_id
        self.name = name
        self.category = category
        self.input = inputs
        self.output = outputs
        self.previous_widgets_id = previous_widgets_id
        self.previous_list = []
        self.next_list = next_list

    def __str__(self):
        return self.name

    def __repr__(self):
        return self.name



def workflows_to_trees():
    all_trees = []
    workflows = Workflow.objects.all()
    for workflow in workflows:
        try:
            print(workflow.name)
            tree = Tree(workflow.name)
            previous_widgets = []
            all_widgets = []
            inps = Input.objects.filter(widget__workflow=workflow).defer('value').prefetch_related('options')
            outs = Output.objects.filter(widget__workflow=workflow).defer('value')
            widgets = workflow.widgets.all().select_related('abstract_widget')
            workflow_links = Workflow.objects.filter(widget__in=widgets)
            inps_by_id = {}
            outs_by_id = {}
            for i in inps:
                if not i.parameter:
                    l = inps_by_id.get(i.widget_id,[])
                    l.append([i.id, i.short_name])
                    inps_by_id[i.widget_id] = l
            for o in outs:
                l = outs_by_id.get(o.widget_id,[])
                l.append([o.id, o.short_name])
                outs_by_id[o.widget_id] = l
            for w in widgets:
                for workflow in workflow_links:
                    if workflow.widget_id == w.id:
                        w.workflow_link_data = workflow
                        w.workflow_link_exists = True
                        break
                else:
                    w.workflow_link_exists = False
                w.inputs_all = inps_by_id.get(w.id,[])
                w.outputs_all = outs_by_id.get(w.id,[])
                previous_ids = [] 
                for c in workflow.connections.all():
                    for i in w.inputs_all:
                        if c.input.id == i[0]:
                            previous_ids.append(c.output.widget.id)
                            
                #print(w.abstract_widget.name, previous_id, w.inputs_all)

                widget_info = {'widget_id': w.id, 'abstract_id':w.abstract_widget.id, 'name':w.abstract_widget.name, 'inputs':w.inputs_all, 
                               'outputs':w.outputs_all, 'category':w.abstract_widget.category, 'previous_ids':previous_ids}
                node = Node(w.id, w.abstract_widget.id, w.abstract_widget.name, w.abstract_widget.category, w.inputs_all, w.outputs_all, previous_ids, [])
                tree.add_node(node)
            all_trees.append(tree)
        except:
            print(workflow, ' :This workflow is weird, leave it alone')
        #tree.print_tree()
    with open(settings.WORKFLOW_TREES_PATH, "wb") as f:
        pickle.dump(all_trees, f)



class GeneticProgramming():

    def __init__(self, initial_population, all_nodes, num_generation=30, mutation_prob=0.2, crossover_prob=0.2):
        self.initial_population = initial_population
        self.population_size = len(initial_population)
        self.num_generation = num_generation
        self.mutation_prob = mutation_prob
        self.crossover_prob = crossover_prob
        self.all_nodes = all_nodes


    def mutation(self, tree):
        mutated = False
        compare = lambda x, y: collections.Counter(x) == collections.Counter(y)
        counter = 0

        #counter makes sure this doesn't run forever if none of the workflow widgets can be replaced
        while not mutated and counter < 100:
            counter += 1
            node = random.choice(tree.all_connected_widgets)
            replacement_candidates = []
            for candidate in self.all_nodes:
                if compare(candidate.input, node.input) and candidate.abstract_id != node.abstract_id:
                    if compare(candidate.output, node.output):
                        replacement_candidates.append(candidate)
            if replacement_candidates:
                mutated = True 
                replacement = random.choice(replacement_candidates) 
                tree.all_connected_widgets.remove(node)
                tree.all_connected_widgets.append(replacement)
                replacement.previous_list = []
                for previous_widget in node.previous_list:
                    previous_widget.next_list.remove(node)
                    previous_widget.next_list.append(replacement)
                    replacement.previous_list.append(previous_widget)
                replacement.next_list = []
                for next_widget in node.next_list:
                    next_widget.previous_list.remove(node)
                    next_widget.previous_list.append(replacement)
                    replacement.next_list.append(next_widget)

        #return false if mutation was not possible   
        if not mutated:
            return False
        return tree

    def crossover(self, tree1, tree2):
        
        #find possible cross over points
        crossover_candidates = []
        for candidate_tree1 in tree1.all_connected_widgets:
            for candidate_tree2 in tree2.all_connected_widgets:
                if candidate_tree1.abstract_id == candidate_tree2.abstract_id:
                    crossover_candidates.append((candidate_tree1, candidate_tree2))

        #choose one point randomly and do crossover
        if not crossover_candidates:
            return False
        crossover_point = random.choice(crossover_candidates)
        candidate_tree1, candidate_tree2 = crossover_candidates
        new_widgets = tree2.get_next_widgets(candidate_tree2, [])
        old_widgets = tree1.get_next_widgets(candidate_tree1, [])
        candidate_tree1.next_list = candidate_tree2.next_list
        for widget in tree1.all_connected_widgets:
            if widget in old_widgets:
                tree1.all_connected_widgets.remove(widget)
        for widget in new_widgets:
            if not widget.next_list:
                tree1.last_widgets.append(widget)
            tree1.all_connected_widgets.append(widget)
        return tree1



















