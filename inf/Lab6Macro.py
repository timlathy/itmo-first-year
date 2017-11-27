import sys
import uno

# --- Main body

def macro_handler(_):
  main()

def main():
  # Obtain the template sheet
  doc = get_libreoffice_document()
  templ = get_templ_sheet(doc)
  # Get the user-specified target date
  target_date_string = get_date_string(templ)
  # Create the target sheet
  target = get_target_sheet(doc, target_date_string)
  # Fetch the schedule
  start_date = parse_date(target_date_string)
  schedule = get_week_schedule(start_date)
  # Insert the schedule   
  format_info_headings(target, start_date)
  fill_in_schedule(templ, target, schedule, start_date)

# --- Date utils

import datetime
def parse_date(date_string):
  return datetime.datetime.strptime(date_string, '%d.%m.%y').date()

def is_even_week(date):
  return int(date.strftime("%V")) % 2 == 0

# --- Data manipulation

def get_week_schedule(start_date):
  week_day = start_date.weekday()
  schedule = fetch_schedule(even_week=is_even_week(start_date))
  week_schedule = list(filter(lambda d: d['day'] >= week_day, schedule))
  return group_weekdays(week_schedule)

import urllib.request
import json
def fetch_schedule(even_week):
  # 0 stands for classes that are scheduled every week, 1 for even weeks, 2 for odd weeks
  week_filter = [0, 1] if even_week else [0, 2]
  # See https://github.com/bukhmastov/CDOITMO/blob/master/app/src/main/java/com/bukhmastov/cdoitmo/network/IfmoRestClient.java
  schedule_api_url = 'http://mountain.ifmo.ru/api.ifmo.ru/public/v1/schedule_lesson_group/P3102'
  api_response = urllib.request.urlopen(schedule_api_url)
  schedule = json.loads(api_response.read().decode('utf-8'))['schedule']
  return list(map(parse_schedule_entry, filter(lambda e: e['data_week'] in week_filter, schedule)))

def parse_schedule_entry(json):
  title_addendum = json['status'] or ''
  title = json['title'] + (' (' + title_addendum + ')') if title_addendum else ''
  return {'title': title,
          'starts_at': json['start_time'],
          'ends_at': json['end_time'] or '',
          'room': json['room'] or '',
          'day': json['data_day']}

from itertools import groupby
def group_weekdays(week):
  day_key = lambda d: d['day']
  return [list(schedule) for _, schedule in groupby(sorted(week, key=day_key), day_key)]

# --- Document manipulation

def get_libreoffice_document():
  desktop = XSCRIPTCONTEXT.getDesktop()
  return desktop.getCurrentComponent()

def get_templ_sheet(doc):
  templ_name = 'H2. Шаблон'
  return doc.getSheets().getByName(templ_name)

def get_target_sheet(doc, date_string):   
  target_name = 'H2. Расписание ' + date_string
  # Insert target sheet
  doc.getSheets().insertNewByName(target_name, doc.getSheets().Count)
  target = doc.getSheets().getByName(target_name)
  # Adjust title column width
  target.getColumns().getByIndex(4).setPropertyValue('Width', 10000)
  return target

def get_date_string(templ):
  return templ.getCellRangeByName('E10').String

def format_info_headings(target, start_date):
  semester_info_range = target.getCellRangeByName('B1:C1')
  week_info_range = target.getCellRangeByName('B2:C2')
  # Align info headings
  alignment = uno.Enum('com.sun.star.table.CellHoriJustify', 'CENTER')
  semester_info_range.setPropertyValue('HoriJustify', alignment)
  week_info_range.setPropertyValue('HoriJustify', alignment)
  # Merge info headings
  semester_info_range.merge(True)
  week_info_range.merge(True)
  # Insert labels
  target.getCellRangeByName('B1').String = 'Семестр'
  target.getCellRangeByName('B2').String = 'Неделя'
  # Insert values
  target.getCellRangeByName('E1').String = 'Осенний' if start_date.month >= 9 else 'Весенний'
  target.getCellRangeByName('E2').String = 'Четная' if is_even_week(start_date) else 'Нечетная'

def fill_in_schedule(templ, target, schedule, start_date):
  copy_from = templ.getCellRangeByName('A3:F8').RangeAddress
  for i in range(0, len(schedule)):
    cell_offset = 3 + 7 * i
    # Copy the schedule-for-a-day table from the template
    copy_to = target.getCellRangeByName('A' + str(cell_offset)).CellAddress
    target.copyRange(copy_to, copy_from)
    # Fill in date cells
    date = start_date + datetime.timedelta(days=i)
    iso_date = date.strftime('%Y-%m-%d')
    for datecol in ['A', 'B']:
      target.getCellRangeByName(datecol + str(cell_offset + 1)).setFormula(iso_date)
    # Fill in individual lessons
    for itemi, item in enumerate(schedule[i]):
      def set_item_cell_value(col, value): 
        target.getCellRangeByName(col + str(cell_offset + 1 + itemi)).String = value
      set_item_cell_value('C', item['starts_at'])
      set_item_cell_value('D', item['ends_at'])
      set_item_cell_value('E', item['title'])
      set_item_cell_value('F', item['room'])

