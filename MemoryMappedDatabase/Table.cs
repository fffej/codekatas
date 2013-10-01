using System.Collections.Generic;
using System.Linq;

namespace MemoryMappedDatabase
{
    public class Table
    {
        private List<Row> m_Rows = new List<Row>(); 

        public TableDefinition Definition { get; private set; }

        public Table(TableDefinition definition)
        {
            Definition = definition;
        }

        public int AddRow(Dictionary<string, IColumnValue> columnValues)
        {
            if (!Definition.VerifyRowIsComplete(columnValues))
            {
                throw new UnfilledRowException();
            }

            var row = new Row(columnValues);
            m_Rows.Add(row);
            return row.RowId;
        }

        public IEnumerable<Row> GetRows()
        {
            return m_Rows;
        }
    }

    public class Row
    {
        private static int s_RowCounter = 0;
        public int RowId { get; private set; }

        public Row(Dictionary<string, IColumnValue> columnValues)
        {
            RowId = s_RowCounter++;
        }
    }
}