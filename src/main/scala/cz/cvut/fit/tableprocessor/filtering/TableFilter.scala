package cz.cvut.fit.tableprocessor.filtering

import cz.cvut.fit.tableprocessor.domain.Table

trait TableFilter {
  // Returns a new table containing only accepted rows.
  def apply(table: Table): Table
}
