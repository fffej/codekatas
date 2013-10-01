using System.Collections.Generic;
using System.Linq;

namespace MemoryMappedDatabase
{
    public class TableDefinition
    {
        protected bool Equals(TableDefinition other)
        {
            return Equals(m_ColumnTypes, other.m_ColumnTypes);
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            if (obj.GetType() != this.GetType()) return false;
            return Equals((TableDefinition) obj);
        }

        public override int GetHashCode()
        {
            return (m_ColumnTypes != null ? m_ColumnTypes.GetHashCode() : 0);
        }

        private readonly Dictionary<string, ColumnType> m_ColumnTypes;

        public TableDefinition(Dictionary<string, ColumnType> columnTypes)
        {
            m_ColumnTypes = columnTypes;
        }

        public int ColumnCount
        {
            get {  return m_ColumnTypes.Count;}
        }


        public bool VerifyRowIsComplete(Dictionary<string, IColumnValue> columnValues)
        {
            var columnNames = m_ColumnTypes.Keys;
            return columnNames.All(columnValues.ContainsKey);
        }
    }
}